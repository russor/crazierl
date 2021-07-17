-module(hook_module).
-export ([parse_transform/2]).

parse_transform(Forms, _Options) ->
	process_forms(Forms, #{}, []).

process_forms([], _, Acc) -> lists:reverse(Acc);

% find hook
process_forms([{attribute, _Anno, hook, Hooks} | T], Map, Acc) ->
	process_forms(T, make_map(Hooks, Map), Acc);

% modify hooked function names
process_forms([{function, Anno, Name, Arity, Clause}| T], Map, Acc) when is_map_key({Name, Arity}, Map) ->
	process_forms(T, Map, [{function, Anno, maps:get({Name, Arity}, Map), Arity, Clause}| Acc]);
process_forms([H | T], Map, Acc) ->
	process_forms(T, Map, [H | Acc]).	

make_map([], Map) -> Map;
make_map([{Fun, Arity} | T], Map) ->
	Real = list_to_atom("real_" ++ atom_to_list(Fun)),
	Hook = list_to_atom("hook_" ++ atom_to_list(Fun)),
	make_map(T, Map#{{Fun, Arity} => Real, {Hook, Arity} => Fun}).
