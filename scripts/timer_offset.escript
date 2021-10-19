#!/usr/bin/env escript

main([Dest]) ->
	Node = list_to_atom(Dest),
	case net_adm:ping(Node) of
		pong -> ok;
		pang ->
			io:format("couldn't contact node ~s, can't check offset code~n", [Node]),
			halt(1)
	end,

	Offset1 = get_offset(Node),
	Offset2 = get_offset(Node),
	Offset3 = get_offset(Node),
	Offset4 = get_offset(Node),
	Offset5 = get_offset(Node),
	io:format("Offsets at ~p (UTC) (ms): ~B, ~B, ~B, ~B, ~b~n", [calendar:universal_time(), Offset1, Offset2, Offset3, Offset4, Offset5]).

get_offset(Node) ->
	Before = os:system_time(),
	Theirs = erpc:call(Node, os, system_time, [], infinity),
	After = os:system_time(),
	Difference = round(((Before + After) / 2.0 ) - Theirs),
	erlang:convert_time_unit(Difference, native, millisecond).
	
	
	
	
