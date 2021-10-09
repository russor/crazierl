#!/usr/bin/env escript

main([Dest | Modules]) -> main(Dest, Modules).

main(Dest, Modules) ->
	Node = list_to_atom(Dest),
	case net_adm:ping(Node) of
		pong -> ok;
		pang ->
			io:format("couldn't contact node ~s, can't push code~n", [Node]),
			halt(1)
	end,
	ModNames = mod_names(Modules, []),
	Md5Mods = fun 
		F([M | T], Acc) ->
			Result = case code:is_loaded(M) of
				false -> case code:get_object_code(M) of
					{M, Bin, _File} -> code:module_md5(Bin);
					_ -> false
				end;
				_ -> M:module_info(md5)
			end,
			F(T, [{M, Result} | Acc]);
		F([], Acc) -> Acc
	end,

	
	Md5s = erpc:call(Node, erlang, apply, [Md5Mods, [ModNames, []]], infinity),
	ToLoad = filter(Md5s, []),
	case ToLoad of
		[] ->
			io:format("no modules changed~n", []),
			halt(0);
		_ ->
			AtomicResult = erpc:call(Node, code, atomic_load, [ToLoad], infinity),
			case AtomicResult of
				ok ->
					io:format("code loaded succssfully, modules: ~p~n",
						  [lists:map(fun({X, _, _}) -> X end, ToLoad)]),
					halt(0);
				_ ->
					io:format("code load failed ~p~n", [AtomicResult]),
					halt(1)
			end
	end.

mod_names([M | T], Acc) ->
	[<<"beam">>, MB, _] = lists:reverse(binary:split(list_to_binary(M), [<<"/">>, <<".">>], [global])),
	Mod = binary_to_atom(MB),
	mod_names(T, [Mod | Acc]);
mod_names([], Acc) -> Acc.

filter([{M, Md5} | T], Acc) ->
	{M, Bin, File} = code:get_object_code(M),
	case code:module_md5(Bin) of
		Md5 -> filter(T, Acc);
		_ -> 
			%io:format("load ~s ~s~n", [M, File]),
			filter(T, [{M, File, Bin} | Acc])
	end;
filter([], Acc) -> Acc.
