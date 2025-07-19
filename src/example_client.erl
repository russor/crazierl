-module (example_client).

% example tcp listener

-export([go/0, go/1]).

go() -> go("192.168.0.10").

go(Ip) ->
	{ok, Sock} = gen_tcp:connect(Ip, 80, [binary]),
	gen_tcp:send(Sock, <<"GET / HTTP/1.0\r\n\r\n">>),
	loop().

loop() ->
	receive
		{tcp, _Socket, Data} ->
			io:format("got ~p~n", [Data]),
			loop();
		{tcp_closed, _Socket} -> ok
	end.
