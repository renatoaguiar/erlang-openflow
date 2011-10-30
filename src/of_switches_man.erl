%% Copyright (c) 2011 Renato Aguiar <renato@aguiar.info>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(of_switches_man).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(TCP_PORT, 6633).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-include("of_proto.hrl").
-record(state, {lsock}).

init([]) ->
    {ok, LSock} = gen_tcp:listen(?TCP_PORT, [binary, {active, false},
                                             {nodelay, true},
                                             {reuseaddr, true},
                                             {packet, raw}]),
    spawn_link(fun() -> accept_loop(LSock) end),
    {ok, #state{lsock=LSock}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ok = gen_tcp:close(State#state.lsock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

accept_loop(LSocket) ->
    AcceptRet = gen_tcp:accept(LSocket),
    spawn(fun() ->
                  {ok, Socket} = AcceptRet,
                  {ok, {Addr, Port}} = inet:peername(Socket),
                  error_logger:info_msg("New connection accepted from ~p:~p~n",
                                        [Addr, Port]),
                  send_message(Socket, #ofp_hello{}),
                  #ofp_hello{} = recv_message(Socket),
                  loop(Socket)
          end),
    accept_loop(LSocket).

loop(Socket) ->
    Msg = recv_message(Socket),
    case Msg of
        #ofp_echo_request{xid=X, payload=P} ->
            send_message(Socket, #ofp_echo_reply{xid=X, payload=P});
        M ->
            spawn(fun() ->
                case process_message(M) of
                    {reply, R} ->
                        send_message(Socket, R);
                    {noreply, R} ->
                        R
                end
            end)
    end,
    loop(Socket).

recv_message(Socket) ->
    {ok, <<Version, Type, Length:16, Xid:32>>} = gen_tcp:recv(Socket, 8),
    Payload = case Length-8 of
        0 ->
            <<>>;
        L ->
            {ok, P} = gen_tcp:recv(Socket, L),
            P
    end,
    of_proto:decode(Version, Type, Xid, Payload).

send_message(Socket, Msg) ->
    gen_tcp:send(Socket, of_proto:encode(Msg)).

process_message(Msg) ->
    case Msg of
        #ofp_packet_in{data=Data, buffer_id=BufferId, in_port=InPort} ->
            {Match, _Payload} = of_proto:parse_headers(Data),
            error_logger:info_msg("Packet In: ~.16B -> ~.16B (~B/~s)~n",
                                  [Match#ofp_match.dl_src,
                                   Match#ofp_match.dl_dst,
                                   Match#ofp_match.tp_dst,
                                   of_match:protocol_name(
                                     Match#ofp_match.nw_proto)]),
            {reply, #ofp_packet_out{
               buffer_id=BufferId, in_port=InPort, data=Data,
               actions=[#ofp_action_output{port=?OFPP_FLOOD}]}};
        _ ->
            {noreply, ok}
    end.
