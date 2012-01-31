%% Copyright (c) 2012 Renato Aguiar <renato@aguiar.info>
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

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, cast/2, call/2]).

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

cast(Method, Args) ->
    gen_server:cast(?SERVER, {Method, Args}).

call(Method, Args) ->
    gen_server:call(?SERVER, {Method, Args}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-include("of_proto.hrl").
-include("of_switches_man.hrl").

-record(state, {listen_socket, switches, handlers}).

init([]) ->
    {ok, Port} = application:get_env(port),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {active, false},
                                             {nodelay, true},
                                             {reuseaddr, true},
                                             {packet, raw}]),
    spawn_link(fun() -> accept_loop(LSock) end),
    {ok, #state{listen_socket=LSock, switches=[], handlers=[]}}.

handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast({send_message, Switch, Message}, State) ->
    gen_tcp:send(Switch#of_switch.socket, of_proto:encode(Message)),
    {noreply, State};
handle_cast({register_handler, Fun}, State) ->
    {noreply, State#state{handlers=[Fun|State#state.handlers]}};
handle_cast({new_switch, Socket}, State) ->
    send_message(Socket, #ofp_hello{}),
    spawn_link(fun() -> recv_loop(Socket) end),
    {ok, {Addr, Port}} = inet:peername(Socket),
    error_logger:info_msg("New connection accepted from ~p:~p~n",
                          [Addr, Port]),
    Switches = [#of_switch{socket=Socket, hosts=[]}|State#state.switches],
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
            [S] = lists:filter(fun(#of_switch{socket=X}) -> X =:= Socket end, Switches),
            {noreply, State#state{switches=[S#of_switch{dpid=Dpid}|Switches -- [S]]}};
        M ->
            [S] = lists:filter(fun(#of_switch{socket=X}) -> X =:= Socket end,
                               State#state.switches),
            lists:foreach(fun(P) -> P(S, M) end, State#state.handlers),
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
