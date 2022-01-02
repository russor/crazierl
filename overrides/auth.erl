-hook([read_cookie/0]).

-include_lib("kernel/src/auth.erl").

hook_read_cookie() ->
	case real_read_cookie() of
		{error, "Failed to create cookie file '/.erlang.cookie': enoent"} ->
			io:format("hooked~n", []),
			crypto:rand_seed(),
			Cookie = lists:flatten(io_lib:format("~s", [lists:map(fun(_X) ->
				rand:uniform(1 + $Z - $A) - 1 + $A end,
				lists:seq(1,20))])),
			{ok, Cookie};
		Other -> Other
	end.