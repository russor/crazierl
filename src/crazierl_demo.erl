-module (crazierl_demo).

-export ([start/0, init/0, new_ip/3]).

start() ->
	spawn(?MODULE, init, []).

init() ->
	Comport = comport:start(16#2F8, "/kern/ioapic/3/0", []),
	Comport ! <<"?">>,
	receive
		{Comport, init_failed} ->
			io:format("crazierl_demo: com2 unavailable, assuming not in crazierl_demo~n");
		{Comport, "!"} ->
			io:format("crazierl_demo: got ! from com2, probably in crazierl_demo!~n"),
			true = register(?MODULE, self()),
			loop(Comport, {});
		{Comport, Other} -> 
			io:format("crazierl_demo: got ~p from com2, giving up~n", [Other])
	end.

loop(Comport, Pending) -> 
	NewPending = receive
		{Comport, [B]} ->
			case Pending of 
				{Cmd} -> {Cmd, B};
				{Cmd, MSB} -> {Cmd, (MSB bsl 8) bor B, []};
				{Cmd, 1, Acc} ->
					command(Comport, Cmd, lists:reverse([B | Acc])),
					{};
				{Cmd, Length, Acc} -> {Cmd, Length - 1, [B | Acc]};
				{} -> {B}
			end;
		{Comport, Other} ->
			io:format("crazierl_demo: got ~p from com2~n", Other),
			Pending;
		{host, Host, Domain} ->
			put(host, {Host, Domain}),
			case erase(cookie) of
				undefined -> ok;
				Cookie -> command(Comport, $c, Cookie)
			end,
			Pending;
		Other ->
			io:format("crazierl_demo: got ~p~n", [Other]),
			Pending
	end,
	loop(Comport, NewPending).

new_ip(Ip, _SubnetMask, _Router) ->
	catch ?MODULE ! {ip, Ip}.

command(Comport, $c, Cookie) ->
	case erase(host) of
		undefined -> put(cookie, Cookie);
		{Host, Domain} ->
			crazierl:start_dist(Host, Domain),
			io:format("setting cookie...~n"),
			erlang:set_cookie(binary_to_atom(binary:encode_hex(list_to_binary(Cookie)))),
			NodeName = atom_to_binary(node()),
			NodeSize = size(NodeName),
			Comport ! <<"n", NodeSize:16, NodeName/binary>>
	end;
command(_Comport, $n, Node) ->
	Result = net_adm:ping(list_to_atom(Node)),
	io:format("ping node ~s -> ~p~n", [Node, Result]);
command(_Comport, Command, Value) ->
	io:format("crazierl_demo: unknown command ~p, value: ~p~n", [Command, Value]).
