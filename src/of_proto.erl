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

-module(of_proto).
-export([decode/4, encode/1, parse_headers/1]).

-include("of_proto.hrl").
-include("of_match.hrl").

-define(OFP_VERSION, 1).
-define(OFPT_HELLO, 0).
-define(OFPT_ECHO_REQUEST, 2).
-define(OFPT_ECHO_REPLY, 3).
-define(OFPT_PACKET_IN, 10).
-define(OFPT_PACKET_OUT, 13).
-define(OFPT_FLOW_MOD, 14).

-define(OFPAT_OUTPUT, 0).

decode(?OFP_VERSION, ?OFPT_HELLO, _Xid, <<>>) ->
    #ofp_hello{};
decode(?OFP_VERSION, ?OFPT_ECHO_REQUEST, Xid, Payload) ->
    #ofp_echo_request{xid=Xid, payload=Payload};
decode(?OFP_VERSION, ?OFPT_PACKET_IN, Xid, Payload) ->
    <<BufferId:32, TotalLen:16, InPort:16,
      Reason, _Pad, Data/binary>> = Payload,
    #ofp_packet_in{xid=Xid, buffer_id=BufferId, total_len=TotalLen,
                   in_port=InPort, reason=Reason, data=Data};
decode(?OFP_VERSION, Type, _Xid, _Payload) ->
    io:format("Can't handle message type ~B~n", [Type]),
    error.

encode(#ofp_hello{}) ->
    <<?OFP_VERSION, ?OFPT_HELLO, 8:16, 0:32>>;
encode(#ofp_echo_reply{xid=Xid, payload=Payload}) ->
    Length = 8 + byte_size(Payload),
    <<?OFP_VERSION, ?OFPT_ECHO_REPLY, Length:16, Xid:32, Payload/binary>>;
encode(#ofp_packet_out{xid=Xid, buffer_id=BufferId, in_port=InPort,
                       actions=Actions, data=Data}) ->
    ActionsData = lists:foldl(fun(A, Acc) ->
                                      B = encode(A),
                                      <<Acc/binary, B/binary>>
                              end, <<>>, Actions),
    ActionsLength = byte_size(ActionsData),
    Length = 16 + ActionsLength + byte_size(Data),
    <<?OFP_VERSION, ?OFPT_PACKET_OUT, Length:16, Xid:32, BufferId:32, InPort:16,
      ActionsLength:16, ActionsData/binary, Data/binary>>;
encode(#ofp_action_output{port=Port, maxlen=MaxLen}) ->
    <<?OFPAT_OUTPUT:16, 8:16, Port:16, MaxLen:16>>.

parse_headers(Payload) ->
    {DLMatch, DLPayload} = parse_dl_header(Payload),
    {NWMatch, NWPayload} = parse_nw_header(DLMatch, DLPayload),
    parse_tp_header(NWMatch, NWPayload).

parse_dl_header(Payload) ->
    {H, P} = of_match:decode_ethernet(Payload),
    {#ofp_match{wildcards=0, dl_src=H#ether.src,
                dl_dst=H#ether.dst, dl_vlan=H#ether.vid,
                dl_vlan_pcp=H#ether.pcp, dl_type=H#ether.proto},
     P}.

parse_nw_header(#ofp_match{dl_type=?IPV4} = Match, Payload) ->
    {H, P} = of_match:decode_ipv4(Payload),
    {Match#ofp_match{nw_tos=H#ipv4.tos, nw_proto=H#ipv4.proto,
                     nw_src=H#ipv4.src, nw_dst=H#ipv4.dst}, P};
parse_nw_header(Match, Payload) ->
    {Match, Payload}.

parse_tp_header(#ofp_match{nw_proto=?ICMP} = Match, Payload) ->
    {H, P} = of_match:decode_icmp(Payload),
    {Match#ofp_match{tp_src=H#icmp.type, tp_dst=H#icmp.code}, P};
parse_tp_header(#ofp_match{nw_proto=?TCP} = Match, Payload) ->
    {H, P} = of_match:decode_tcp(Payload),
    {Match#ofp_match{tp_src=H#tcp.src, tp_dst=H#tcp.dst}, P};
parse_tp_header(#ofp_match{nw_proto=?UDP} = Match, Payload) ->
    {H, P} = of_match:decode_udp(Payload),
    {Match#ofp_match{tp_src=H#udp.src, tp_dst=H#udp.dst}, P};
parse_tp_header(Match, Payload) ->
    {Match, Payload}.
