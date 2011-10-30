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

-record(ofp_hello, {}).
-record(ofp_echo_request, {xid=0, payload}).
-record(ofp_echo_reply, {xid=0, payload}).
-record(ofp_packet_in, {xid=0, buffer_id, total_len, in_port, reason, data}).
-record(ofp_packet_out, {xid=0, buffer_id, in_port, actions, data}).
-record(ofp_flow_mod, {}).
-record(ofp_match, {wildcards, in_port, dl_src, dl_dst, dl_vlan,
                    dl_vlan_pcp, dl_type, nw_tos, nw_proto, nw_src, nw_dst,
                    tp_src, tp_dst}).

-record(ofp_action_output, {port, maxlen=0}).

-define(OFPP_FLOOD, 16#fffb).
