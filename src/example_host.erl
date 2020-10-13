-module (example_host).

% example tcp listener

-export([start/0, init/1]).
-export([handle_info/2]).
-export([worker_loop/2]).
-behavior(gen_server).

start() ->
	tcp:start(),
	case gen_server:start({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> Pid;
		{error, {already_started, Pid}} -> Pid;
		Other -> Other
	end.

init([]) ->
	true = tcp:listen(80, self()),
	{ok, {}}.


handle_info({tcp_open, Socket, Bin}, State) ->
	Worker = spawn(example_host, worker_loop, [Socket, Bin]),
	tcp:controlling_process(Socket, Worker),
	{noreply, State}.

worker_loop(Socket, Bin) ->
	case erlang:decode_packet(http_bin, Bin, []) of
		{more, _} -> receive {tcp, Socket, Data} ->
				worker_loop(Socket, <<Bin/binary, Data/binary>>)
			end;
		{ok, {http_request, Method, Uri, Version}, Rest} ->
			header_loop(Socket, {Method, Uri, Version}, [], Rest)
	end.

header_loop(Socket, Request, Headers, Bin) ->
	case erlang:decode_packet(httph_bin, Bin, []) of
		{more, _} -> receive {tcp, Socket, Data} ->
				header_loop(Socket, Request, Headers, <<Bin/binary, Data/binary>>)
			end;
		{ok, http_eoh, _Rest} ->
			output(Socket, Request, lists:reverse(Headers));
		{ok, {http_header, Int, Field, OrigField, Val}, Rest} ->
			header_loop(Socket, Request, [{Int, Field, OrigField, Val}|Headers], Rest)
	end.

output(Socket, {Method, Uri, Version}, Headers) ->
	Response = iolist_to_binary(io_lib:format(
		"HTTP/1.0 200 OK\r\nConnection: close\r\nContent-Type: text/plain\r\n\r\n"
		"Your method: ~s\r\n"
	        "Your URI: ~w\r\n"
	        "Your version: ~w\r\n", [Method, Uri, Version])),
	tcp:send(Socket, Response),
	tcp:close(Socket).
