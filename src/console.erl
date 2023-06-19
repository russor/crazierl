-module (console).
-export ([start/1, init/1]).
-record (s, {
	ports,
	fd
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
	{ok, FD} = gen_udp:open(0, [
	        {inet_backend, inet},
		{ifaddr, {local, "/kern/fd/std"}},
		{active, true},
		binary
	]),
	loop(State#s{fd = FD}).

loop (State = #s{ports = Ports, fd = FD}) ->
	receive
		{udp, FD, _, _, Data} ->
			lists:foreach (fun (P) -> P ! Data end, Ports);
		{_Port, Data} ->
			gen_udp:send(FD, Data);
		Other ->
			io:format("console unexpected input ~p~n", [Other])
	end,
	loop(State).
