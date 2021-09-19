-module (example_host).

% example tcp listener

-export([start/0]).
-export([worker_loop/2]).

start() ->
	spawn(fun init/0).

init() ->
	Sock = etcpip_socket:listen(80),
	io:format("Sock ~w~n", [Sock]),
	accept_loop(Sock).

accept_loop(ListenSock) ->
	{ok, Socket} = etcpip_socket:accept(ListenSock),
	_Worker = spawn(?MODULE, worker_loop, [Socket, <<>>]),
	accept_loop(ListenSock).

worker_loop(Socket, Bin) ->
	case erlang:decode_packet(http_bin, Bin, []) of
		{more, _} -> 
			{ok, Data} = etcpip_socket:recv(Socket, 0, [], infinity),
			worker_loop(Socket, <<Bin/binary, Data/binary>>);
		{ok, {http_request, Method, Uri, Version}, Rest} ->
			header_loop(Socket, {Method, Uri, Version}, [], Rest)
	end.

header_loop(Socket, Request, Headers, Bin) ->
	case erlang:decode_packet(httph_bin, Bin, []) of
		{more, _} -> 
			{ok, Data} = etcpip_socket:recv(Socket, 0, [], infinity),
			header_loop(Socket, Request, Headers, <<Bin/binary, Data/binary>>);
		{ok, http_eoh, _Rest} ->
			output(Socket, Request, lists:reverse(Headers));
		{ok, {http_header, Int, Field, OrigField, Val}, Rest} ->
			header_loop(Socket, Request, [{Int, Field, OrigField, Val}|Headers], Rest)
	end.

output(Socket, {_Method, {abs_path, <<"/process_info">>}, _Version}, _Headers) ->
	Response = <<"HTTP/1.0 200 OK\r\nConnection: close\r\nContent-Type: text/plain\r\n\r\n">>,
	etcpip_socket:send(Socket, Response, [], infinity),
	lists:foreach(fun (P) ->
		etcpip_socket:send(Socket, iolist_to_binary(io_lib:format(
			"~n~p~n~p~n", [P, process_info(P)])), [], infinity)
	end, processes()),

	etcpip_socket:close(Socket);

output(Socket, {_Method, {abs_path, <<"/processes">>}, _Version}, _Headers) ->
	Response = iolist_to_binary([
		"HTTP/1.0 200 OK\r\nConnection: close\r\nContent-Type: text/plain\r\n\r\n",
	        io_lib:format("~B", [length(processes())]), "\r\n"
	        ]),
	etcpip_socket:send(Socket, Response, [], infinity),
	etcpip_socket:close(Socket);

output(Socket, {_Method, {abs_path, <<"/memory">>}, _Version}, _Headers) ->
	Response = iolist_to_binary([
		"HTTP/1.0 200 OK\r\nConnection: close\r\nContent-Type: text/plain\r\n\r\n",
	        io_lib:format("~w", [erlang:memory()]), "\r\n"
	        ]),

	etcpip_socket:send(Socket, Response, [], infinity),
	etcpip_socket:close(Socket);

output(Socket, {_Method, _Uri, _Version}, _Headers) ->
	Response = <<"HTTP/1.0 404 Not Found\r\nConnection: close\r\nContent-Type: text/plain\r\n\r\n"
		"Not found\r\n">>,
	etcpip_socket:send(Socket, Response, [], infinity),
	etcpip_socket:close(Socket).
