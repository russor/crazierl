-hook([open/4, setopt/3, getopt/2, bind/2, listen/2, accept/2, connect/3,
       recv/4, send/4, sendto/5, recvfrom/4,
       close/1, sockname/1, peername/1, info/1, cancel/2]).
-include_lib("kernel/src/socket.erl").

hook_open(inet, stream, tcp, Opts) ->
	Sock = etcpip_socket:open(tcp, Opts),
	{ok, {etcpip, Sock}};
hook_open(inet, dgram, udp, Opts) ->
	Sock = etcpip_socket:open(udp, Opts),
	{ok, {etcpip, Sock}};
hook_open(Domain, Type, Protocol, Opts) -> real_open(Domain, Type, Protocol, Opts).

hook_setopt({etcpip, Sock}, Opt, Val) -> etcpip_socket:setopt(Sock, Opt, Val);
hook_setopt(Sock, Opt, Val) -> real_setopt(Sock, Opt, Val).

hook_getopt({etcpip, Sock}, Opt) -> etcpip_socket:getopt(Sock, Opt);
hook_getopt(Sock, Opt) -> real_getopt(Sock, Opt).

hook_bind({etcpip, Sock}, Addr) -> etcpip_socket:bind(Sock, Addr);
hook_bind(Sock, Addr) -> real_bind(Sock, Addr).

hook_listen({etcpip, Sock}, Backlog) -> etcpip_socket:listen(Sock, Backlog);
hook_listen(Sock, Backlog) -> real_listen(Sock, Backlog).

hook_accept({etcpip, Sock}, Timeout) ->
	case etcpip_socket:accept(Sock, Timeout) of
		{ok, NewSock} -> {ok, {etcpip, NewSock}};
		O -> O
	end;
hook_accept(Sock, Timeout) -> real_accept(Sock, Timeout).

hook_connect({etcpip, Sock}, SockAddr, Timeout) ->
	etcpip_socket:connect(Sock, SockAddr, Timeout);
hook_connect(Sock, SockAddr, Timeout) -> real_connect(Sock, SockAddr, Timeout).

hook_recv({etcpip, Sock}, Length, Options, Timeout) -> etcpip_socket:recv(Sock, Length, Options, Timeout);
hook_recv(Sock, Length, Options, Timeout) -> real_recv(Sock, Length, Options, Timeout).

hook_recvfrom({etcpip, Sock}, Length, Options, Timeout) -> etcpip_socket:recvfrom(Sock, Length, Options, Timeout);
hook_recvfrom(Sock, Length, Options, Timeout) -> real_recvfrom(Sock, Length, Options, Timeout).

hook_send({etcpip, Sock}, Data, Flags, Timeout) -> etcpip_socket:send(Sock, Data, Flags, Timeout);
hook_send(Sock, Data, Flags, Timeout) -> real_send(Sock, Data, Flags, Timeout).

hook_sendto({etcpip, Sock}, Data, Dest, Flags, Timeout) -> etcpip_socket:sendto(Sock, Data, Dest, Flags, Timeout);
hook_sendto(Sock, Data, Dest, Flags, Timeout) -> real_sendto(Sock, Data, Dest, Flags, Timeout).

hook_close({etcpip, Sock}) -> etcpip_socket:close(Sock);
hook_close(Sock) -> real_close(Sock).

hook_sockname({etcpip, Sock}) -> etcpip_socket:sockname(Sock);
hook_sockname(Sock) -> real_sockname(Sock).

hook_peername({etcpip, Sock}) -> etcpip_socket:peername(Sock);
hook_peername(Sock) -> real_peername(Sock).

hook_info({etcpip, Sock}) -> etcpip_socket:info(Sock);
hook_info(Sock) -> real_info(Sock).

hook_cancel({etcpip, Sock}, Info) -> etcpip_socket:cancel(Sock, Info);
hook_cancel(Sock, Info) -> real_cancel(Sock, Info).
