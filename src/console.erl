-module (console).
-export ([start/1, init/1]).
-record (s, {
	ports,
	stdin,
	stderr,
	stdout
}).

start(Ports) ->
	spawn(?MODULE, init, [Ports]).

init (Ports) ->
	init (Ports, #s{ports = []}).

init ([], State) -> open_console(State);
init ([{M, F, A} | Ports], #s{ports = P}) ->
	init(Ports, #s{ports = [apply(M, F, A) | P]}).

open_console(#s{ ports = []}) ->
	error_logger:error_msg("no ports for console");
open_console(State) ->
	{ok, STDIN} = gen_udp:open(0, [
	        {inet_backend, inet},
		{ifaddr, {local, "/kern/fd/0"}},
		{active, true},
		binary
	]),
	{ok, STDOUT} = gen_udp:open(0, [
	        {inet_backend, inet},
		{ifaddr, {local, "/kern/fd/1"}},
		{active, true},
		binary
	]),
	{ok, STDERR} = gen_udp:open(0, [
	        {inet_backend, inet},
		{ifaddr, {local, "/kern/fd/2"}},
		{active, true},
		binary
	]),
%	STDOUT = STDERR = {},
	loop(State#s{stdin = STDIN, stdout = STDOUT, stderr = STDERR}).

loop (State = #s{ports = Ports, stdin = IN, stdout = OUT, stderr = ERR}) ->
	receive
		{udp, IN, _, _, Data} -> % it's weird to get a write to STDIN, but it happens
			lists:foreach (fun (P) -> P ! {stdin, Data} end, Ports);
		{udp, OUT, _, _, Data} ->
			lists:foreach (fun (P) -> P ! {stdout, Data} end, Ports);
		{udp, ERR, _, _, Data} ->
			lists:foreach (fun (P) -> P ! {stderr, Data} end, Ports);
		{_Port, Data} ->
			gen_udp:send(IN, Data);
		Other ->
			io:format("console unexpected input ~p~n", [Other])
	end,
	loop(State).
