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

-module(of_match).
-export([decode_ethernet/1, decode_ipv4/1, decode_udp/1, decode_icmp/1,
         decode_tcp/1, protocol_name/1]).

-include("of_match.hrl").

decode_ethernet(<<Dst:48, Src:48, 16#8100:16, PCP:3, _CFI:1, VID:12, Proto:16,
                  Payload/binary>>) ->
    {#ether{dst=Dst, src=Src, pcp=PCP, vid=VID, proto=Proto}, Payload};
decode_ethernet(<<Dst:48, Src:48, Proto:16, Payload/binary>>) ->
    {#ether{dst=Dst, src=Src, pcp=0, vid=16#ffff, proto=Proto}, Payload}.

decode_ipv4(<<4:4, IHL:4, DSCP:8, _TotalLength:16, _Id:16, _Flags:3,
              _FragmentOffset:13, _TTL, Proto, _Checksum:16, Src:32, Dst:32,
              Rest/binary>>) ->
    L = (IHL - 5) * 32,
    <<_Options:L, P/binary>> = Rest,
    {#ipv4{tos=DSCP, proto=Proto, src=Src, dst=Dst}, P}.

decode_udp(<<Src:16, Dst:16, _Lenght:16, _Checksum:16, Rest/binary>>) ->
    {#udp{src=Src, dst=Dst}, Rest}.

decode_icmp(<<Type:8, Code:8, _Checksum:16, _Header:32, Rest/binary>>) ->
    {#icmp{type=Type, code=Code}, Rest}.

decode_tcp(<<Src:16, Dst:16, _SeqNum:32, _AckNum:32, Offset:4, _Reserved:3,
             _Flags:9, _WinSize:16, _Checksum:16, _Urgent:16, Rest/binary>>) ->
    L = (Offset - 5) * 32,
    <<_Options:L, P/binary>> = Rest,
    {#tcp{src=Src, dst=Dst}, P}.

protocol_name(?IPV4) ->
    "ipv4";
protocol_name(?ARP) ->
    "arp";
protocol_name(?ICMP) ->
    "icmp";
protocol_name(?TCP) ->
    "tcp";
protocol_name(?UDP) ->
    "udp".
