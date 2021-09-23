#!/usr/bin/env escript

main(_) ->
	crypto:rand_seed(),
	io:format("~s", [lists:map(fun(_X) ->
		rand:uniform(1 + $Z - $A) - 1 + $A end,
		lists:seq(1,20))]).