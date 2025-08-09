-module (dhcpc).

-export ([go/0]).
-define (CLIENT_PORT, 68).
-define (SERVER_PORT, 67).
-define (BROADCAST, 16#FFFFFFFF).

go() ->

    Sock = udp:open(?CLIENT_PORT),
    Xid = crypto:strong_rand_bytes(4),
    Terms = application:get_all_env(etcpip),
    {value, {mac, Mac}} = lists:keysearch(mac, 1, Terms),
    Discovery = << 1, 1, 6, 0, % op, hytpe, hlen, hops
                 Xid/binary, % xid
                 0:16, 0:16, % secs, flags
                 0:32, %client address
                 0:32, % your address
                 0:32, % server address
                 0:32, % relay address
                 Mac/binary, 0:(128 - size(Mac) * 8), % hardware address
                 0:512, % sname
                 0:1024, % file
                 99, 130, 83, 99, % magic cookie
                 53, 1, 1, % DHCP Discover
                 55, 3, 3, 12, 15, % parameter request Router, Host Name, Domain Name
                 255 % End
              >>,
    udp:send(0, ?CLIENT_PORT, ?BROADCAST, ?SERVER_PORT, Discovery),
    Options = receive
       {udp, {_, ?CLIENT_PORT, _, ?SERVER_PORT},
             << 2, 1, 6, _Hops,
                Xid:4/binary,
                _Secs:16, _Flags:16, _CIAddr:32,
                MyAddr:32, _SIAddr:32, _GIAddr:32,
                Chaddr:128, Sname:64/binary, File:128/binary,
                99, 130, 83, 99,
                RawOptions/binary>>} ->
           parse_options(RawOptions, #{})
    end,
    ServerId = maps:get(server_id, Options),
    Request = << 1, 1, 6, 0, % op, hytpe, hlen, hops
                 Xid/binary, % xid
                 0:16, 0:16, % secs, flags
                 0:32, %client address
                 MyAddr:32, % your address
                 0:32, % server address
                 0:32, % relay address
                 Mac/binary, 0:(128 - size(Mac) * 8), % hardware address
                 0:512, % sname
                 0:1024, % file
                 99, 130, 83, 99, % magic cookie
                 54, 4, ServerId:32, % Server ID
                 53, 1, 3, % DHCP Request
                 50, 4, MyAddr:32, % Requested IP Address
                 255 % DHCP Discover
              >>,
    udp:send(0, ?CLIENT_PORT, ?BROADCAST, ?SERVER_PORT, Request),
    receive
       {udp, {_, ?CLIENT_PORT, _, ?SERVER_PORT},
             << 2, 1, 6, _Hops,
                Xid:4/binary,
                _Secs:16, _Flags:16, _CIAddr:32,
                MyAddr:32, _SIAddr:32, _GIAddr:32,
                Chaddr:128, Sname:64/binary, File:128/binary,
                99, 130, 83, 99,
                RawOptions2/binary>>} ->
           O2 = parse_options(RawOptions2, #{}),
           io:format("Ip ~p, Netmask ~p, Gateway ~p~n", [etcpip_socket:unmap_ip(MyAddr), etcpip_socket:unmap_ip(maps:get(subnet_mask, O2)), etcpip_socket:unmap_ip(maps:get(router, O2))]),
           case {maps:get(hostname, O2, undefined), maps:get(domain_name, O2, undefined)} of
                {undefined, undefined} ->
                    NodeName = io_lib:format("crazierl@~8.16.0B.nip.io", [MyAddr]),
                    net_kernel:start([binary_to_atom(iolist_to_binary(NodeName)), longnames]);
                {Hostname, undefined} ->
                    NodeName = io_lib:format("crazierl@~s", [Hostname]),
                    net_kernel:start([binary_to_atom(iolist_to_binary(NodeName)), shortnames]);
                {Hostname, Domain} ->
                    NodeName = io_lib:format("crazierl@~s.~s", [Hostname, Domain]),
                    net_kernel:start([binary_to_atom(iolist_to_binary(NodeName)), longnames])
           end,
           case maps:get(dns_server, O2, etcpip_socket:map_ip({8,8,8,8})) of
               ServerInt when is_integer(ServerInt) ->
                   DNSIp = etcpip_socket:unmap_ip(ServerInt),
                   io:format("dns server ~p~n", [DNSIp]),
                   inet_db:add_ns(DNSIp);
               _ -> io:format("no dns server from dhcpd~n")
           end,

           etcpip_socket:new_ip(MyAddr, maps:get(subnet_mask, O2), maps:get(router, O2));
        M -> io:format("got ~w~n", [M])
    end.

parse_options(<<>>, Map) -> Map;
% DHCP Message Type
parse_options(<<53, 1, 1, Rest/binary>>, Map) -> parse_options(Rest, Map#{message => discover});
parse_options(<<53, 1, 2, Rest/binary>>, Map) -> parse_options(Rest, Map#{message => offer});
parse_options(<<53, 1, 3, Rest/binary>>, Map) -> parse_options(Rest, Map#{message => request});
parse_options(<<53, 1, 4, Rest/binary>>, Map) -> parse_options(Rest, Map#{message => decline});
parse_options(<<53, 1, 5, Rest/binary>>, Map) -> parse_options(Rest, Map#{message => ack});
parse_options(<<53, 1, 6, Rest/binary>>, Map) -> parse_options(Rest, Map#{message => nak});
parse_options(<<53, 1, 7, Rest/binary>>, Map) -> parse_options(Rest, Map#{message => release});
parse_options(<<53, 1, 8, Rest/binary>>, Map) -> parse_options(Rest, Map#{message => inform});
% Subnet Mask
parse_options(<<1, 4, Mask:32, Rest/binary>>, Map) -> parse_options(Rest, Map#{subnet_mask => Mask});
% Router
parse_options(<<3, N, PrimaryRouter:32, _OtherRouters:(N - 4)/binary, Rest/binary>>, Map) when N rem 4 == 0 ->
    parse_options(Rest, Map#{router => PrimaryRouter});
% Domain Name Server
parse_options(<<6, N, PrimaryDNS:32, _OtherDNS:(N - 4)/binary, Rest/binary>>, Map) when N rem 4 == 0 ->
    parse_options(Rest, Map#{dns_server => PrimaryDNS});
% Lease Time
parse_options(<<51, 4, Time:32, Rest/binary>>, Map) -> parse_options(Rest, Map#{lease_time => Time});
% Server Identifier
parse_options(<<54, 4, IP:32, Rest/binary>>, Map) -> parse_options(Rest, Map#{server_id => IP});
% Hostname
parse_options(<<12, Len, Name:Len/binary, Rest/binary>>, Map) -> parse_options(Rest, Map#{hostname => Name});
% Domain name
parse_options(<<15, Len, Name:Len/binary, Rest/binary>>, Map) -> parse_options(Rest, Map#{domain_name => Name});
parse_options(<<0, Rest/binary>>, Map) -> parse_options(Rest, Map);
parse_options(<<255, _/binary>>, Map) -> Map;
parse_options(<<N, Len, Data:Len/binary, Rest/binary>>, Map) -> parse_options(Rest, Map#{{unknown, N} => Data}).

