%%%-------------------------------------------------------------------
%%% File    : eth_port.erl
%%% Author  : Javier Paris Fernandez <javier.paris@udc.es>
%%% Description : Ethernet Port driver
%%%
%%% Created :  2 Aug 2004 by Javier Paris Fernandez <javier.paris@udc.es>
%%%
%%% Modified: 2020 by Richard Russo <toast@ruka.org>
%%%
%%% erlang-tcpip, Copyright (C) 2004 Javier Paris
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
-module(eth_port).

% API
-export([start_reader/1, start_writer/0, send/1, get_instance/0, get_mtu/0]).

%--- API -----------------------------------------------------------------------

start_reader(Iface) ->
    etcpip_proc:start_link(eth_port_reader, #{
        init        => fun() -> ok end,
        handle_call => fun reader_call/3
    }).

start_writer() ->
    etcpip_proc:start_link(eth_port_writer, #{
    	init        => fun() -> ok end,
        handle_cast => fun writer_cast/2
    }).

send(Packet) -> etcpip_proc:cast(eth_port_writer, {send, Packet}).

get_instance() -> etcpip_proc:call(eth_port_reader, get_instance).

get_mtu() -> {mtu, etcpip_proc:call(eth_port_reader, get_mtu)}.

%--- Reader --------------------------------------------------------------------

reader_call(get_instance, _From, State) ->
    {reply, {self()}, State};
reader_call(get_mtu, _From, State) ->
    %<<MTU:32/native-integer>> = list_to_binary(port_control(Port, 2, [])),
    {reply, 576, State}.

%--- Writer --------------------------------------------------------------------

writer_init() -> eth_port:get_instance().

writer_cast({send, Packet}, State) ->
    gen_server:call(rtl_8168, {send, Packet}),
    {noreply, State}.
