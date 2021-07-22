-hook([open/4]).
-include_lib("kernel/src/socket.erl").

hook_open(Domain, Type, Protocol, Opts) ->
	io:format("Domain ~p, Type ~p, Protocol ~p, Opts ~p~n",
		[Domain, Type, Protocol, Opts]),
	real_open(Domain, Type, Protocol, Opts).
