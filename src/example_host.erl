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
	Socket = etcpip_socket:accept(ListenSock),
	Worker = spawn(example_host, worker_loop, [Socket, <<>>]),
	accept_loop(ListenSock).

worker_loop(Socket, Bin) ->
	case erlang:decode_packet(http_bin, Bin, []) of
		{more, _} -> 
			Data = etcpip_socket:recv(Socket, 4096),
			worker_loop(Socket, <<Bin/binary, Data/binary>>);
		{ok, {http_request, Method, Uri, Version}, Rest} ->
			header_loop(Socket, {Method, Uri, Version}, [], Rest)
	end.

header_loop(Socket, Request, Headers, Bin) ->
	case erlang:decode_packet(httph_bin, Bin, []) of
		{more, _} -> 
			Data = etcpip_socket:recv(Socket, 4096),
			header_loop(Socket, Request, Headers, <<Bin/binary, Data/binary>>);
		{ok, http_eoh, _Rest} ->
			output(Socket, Request, lists:reverse(Headers));
		{ok, {http_header, Int, Field, OrigField, Val}, Rest} ->
			header_loop(Socket, Request, [{Int, Field, OrigField, Val}|Headers], Rest)
	end.

output(Socket, {Method, Uri, Version}, Headers) ->
	Response = iolist_to_binary([io_lib:format(
		"HTTP/1.0 200 OK\r\nConnection: close\r\nContent-Type: text/plain\r\n\r\n"
		"Your method: ~s\r\n"
	        "Your URI: ~s\r\n"
	        "Your version: ~w\r\n", [Method, clean(Uri), Version]),
	        "\r\n",
	        lists:map(fun({_, _, K, V}) -> [K, ": ", V, "\r\n"] end, Headers),
	        io_lib:format("~w", [erlang:memory()]), "\r\n"
	        ]),

	etcpip_socket:send(Socket, Response),
	lists:foreach(fun (P) ->
		etcpip_socket:send(Socket, iolist_to_binary(io_lib:format(
			"~n~p~n~p~n", [P, process_info(P)])))
	end, processes()),

	etcpip_socket:close(Socket).

clean('*') -> '*';
clean({absoluteURI, Scheme, Host, Port, Path}) ->
	P = case Port of
		undefined -> "";
		_ -> io_lib:format(":~B", [Port])
	end,
	io_lib:format("~s://~s~s~s", [Scheme, Host, P, Path]);
clean({scheme, Scheme, String}) ->
	io_lib:format("~s:~s", [Scheme, String]);
clean({abs_path, Path}) -> Path.
