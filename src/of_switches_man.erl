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

%% TODO Rename of_switches_man to of_sockets
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

-record(switch, {socket, dpid, hosts}).
-record(state, {listen_socket, switches}).

init([]) ->
    {ok, LSock} = gen_tcp:listen(?TCP_PORT, [binary, {active, false},
                                             {nodelay, true},
                                             {reuseaddr, true},
                                             {packet, raw}]),
    spawn_link(fun() -> accept_loop(LSock) end),
    {ok, #state{listen_socket=LSock, switches=[]}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({new_switch, Socket}, State) ->
    send_message(Socket, #ofp_hello{}),
    spawn_link(fun() -> recv_loop(Socket) end),
    {ok, {Addr, Port}} = inet:peername(Socket),
    error_logger:info_msg("New connection accepted from ~p:~p~n",
                          [Addr, Port]),
    Switches = [#switch{socket=Socket, hosts=[]}|State#state.switches],
    {noreply, State#state{switches=Switches}};
handle_cast({switch_message, Socket, Msg}, State) ->
    case Msg of
        #ofp_echo_request{xid=X, payload=P} ->
            send_message(Socket, #ofp_echo_reply{xid=X, payload=P}),
            {noreply, State};
        #ofp_hello{} ->
            send_message(Socket, #ofp_features_request{}),
            {noreply, State};
        #ofp_features_reply{dpid=Dpid} ->
            Switches = State#state.switches,
            [S] = lists:filter(fun(#switch{socket=X}) -> X =:= Socket end, Switches),
            {noreply, State#state{switches=[S#switch{dpid=Dpid}|Switches -- [S]]}};
        #ofp_packet_in{data=Data, buffer_id=BufferId, in_port=InPort} ->
            {TPMatch, _Payload} = of_proto:parse_headers(Data),
            Match = TPMatch#ofp_match{in_port = InPort},
            error_logger:info_msg("New Flow: ~.16B -> ~.16B (~p/~s)~n",
                                  [Match#ofp_match.dl_src,
                                   Match#ofp_match.dl_dst,
                                   Match#ofp_match.tp_dst,
                                   of_match:protocol_name(
                                     Match#ofp_match.nw_proto)]),
            Switches = State#state.switches,
            [S] = lists:filter(fun(#switch{socket=X}) -> X =:= Socket end, Switches),
            case lists:keyfind(Match#ofp_match.dl_dst, 1, S#switch.hosts) of
                false ->
                    send_message(Socket, #ofp_packet_out{
                                   buffer_id=BufferId, in_port=InPort, data=Data,
                                   actions=[#ofp_action_output{port=?OFPP_FLOOD}]});
                {_, Port} ->
                    send_message(Socket, #ofp_flow_mod{
                                   match=Match, command=modify, hard_timeout=30,
                                   actions=[#ofp_action_output{port=Port}]}),
                    send_message(Socket, #ofp_packet_out{
                                   buffer_id=BufferId, in_port=InPort, data=Data,
                                   actions=[#ofp_action_output{port=Port}]})
            end,
            {noreply, State#state{switches=[S#switch{hosts=[{Match#ofp_match.dl_src, InPort}|S#switch.hosts]}|Switches -- [S]]}};
        M ->
            error_logger:info_msg("Unhandled message: ~p~n", [M]),
            {noreply, State}

    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ok = gen_tcp:close(State#state.listen_socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

accept_loop(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(?SERVER, {new_switch, Socket}),
    accept_loop(LSocket).

recv_loop(Socket) ->
    Msg = recv_message(Socket),
    gen_server:cast(?SERVER, {switch_message, Socket, Msg}),
    recv_loop(Socket).

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
