-module (example_client).

% example tcp listener

-export([go/0, go/1]).

go() -> go("crazierl.org").

go(Ip) ->
	{ok, Sock} = gen_tcp:connect(Ip, 80, [binary]),
	% Note! we're not sending a Host header,
	% this is to try to keep the response small
	gen_tcp:send(Sock, <<"GET / HTTP/1.0\r\n\r\n">>),
	loop().

loop() ->
	receive
		{tcp, _Socket, Data} ->
			io:format("got ~p~n", [Data]),
			loop();
		{tcp_closed, _Socket} -> ok
	end.
