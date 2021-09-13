-hook([open/4, setopt/3, bind/2, listen/2, accept/2]).
-include_lib("kernel/src/socket.erl").

hook_open(inet, stream, tcp, Opts) ->
	Sock = etcpip_socket:open(tcp, Opts),
	{ok, {etcpip, Sock}};
hook_open(Domain, Type, Protocol, Opts) -> real_open(Domain, Type, Protocol, Opts).

hook_setopt({etcpip, Sock}, Opt, Val) -> etcpip_socket:setopt(Sock, Opt, Val);
hook_setopt(Sock, Opt, Val) -> real_setopt(Sock, Opt, Val).

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
