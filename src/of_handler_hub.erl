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

-module(of_handler_hub).
-export([handle_message/2]).

-include("of_proto.hrl").
-include("of_match.hrl").

handle_message(Switch, #ofp_packet_in{data=Data, buffer_id=BufferId,
                                      in_port=InPort}) ->
    {TPMatch, _Payload} = of_proto:parse_headers(Data),
    Match = TPMatch#ofp_match{in_port = InPort},
    case is_valid_proto(Match) of
        true ->
            log_flow(Match),
            send_message(Switch, #ofp_flow_mod{
                           match=Match, command=modify, idle_timeout=10,
                           actions=[#ofp_action_output{port=?OFPP_FLOOD}]}),
            send_message(Switch, #ofp_packet_out{
                           buffer_id=BufferId, in_port=InPort, data=Data,
                           actions=[#ofp_action_output{port=?OFPP_TABLE}]});
        false ->
            send_message(Switch, #ofp_packet_out{
                           buffer_id=BufferId, in_port=InPort, data=Data,
                           actions=[#ofp_action_output{port=?OFPP_FLOOD}]})
    end,
    ok;
handle_message(_Switch, _Message) ->
    ok.

log_flow(Match) ->
    error_logger:info_msg("New Flow: ~.16B -> ~.16B (~p/~s)~n",
                          [Match#ofp_match.dl_src,
                           Match#ofp_match.dl_dst,
                           Match#ofp_match.tp_dst,
                           of_match:protocol_name(
                             Match#ofp_match.nw_proto)]).

send_message(Switch, Message) ->
    gen_server:cast(of_switches_man, {send_message, Switch, Message}).

is_valid_proto(#ofp_match{dl_type=?IPV4, nw_proto=?TCP}) ->
    true;
is_valid_proto(#ofp_match{dl_type=?IPV4, nw_proto=?UDP}) ->
    true;
is_valid_proto(#ofp_match{dl_type=?IPV4, nw_proto=?ICMP}) ->
    true;
is_valid_proto(Match) when is_record(Match, ofp_match) ->
    false.
