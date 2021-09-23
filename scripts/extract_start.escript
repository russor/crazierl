#!/usr/bin/env escript

main([File]) ->
	{ok, Contents} = file:read_file(File),
	{ok, Tokens, _End} = erl_scan:string(binary_to_list(Contents)),
	{ok, Term} = erl_parse:parse_term(Tokens),
	{script, _Version, Things} = Term,
	lists:foreach(fun
		({path, Paths}) -> 
			io:format("path\t~s~n", [string:join(Paths, "\t")]);
		({primLoad, Modules}) ->
			ModString = lists:map(fun erlang:atom_to_list/1, Modules),
			io:format("primLoad\t~s~n", [string:join(ModString, "\t")]);
		(_) -> ok
	end, Things).
