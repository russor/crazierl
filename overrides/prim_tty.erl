-hook([tgetstr/1]).

-include_lib("kernel/src/prim_tty.erl").


hook_tgetstr(Char) ->
	case real_tgetstr(Char) of
		{ok, Ret} -> {ok, filter(<<>>, Ret)};
		O -> O
	end.

filter(Clean, <<>>) -> Clean;
filter(Clean, <<$$, $<, Rest/binary>>) ->
	case filter_end(<<>>, Rest) of
		false -> <<Clean/binary, $$, $<, Rest/binary>>;
		NewRest -> filter(<<Clean/binary>>, NewRest)
	end;
filter(Clean, <<X, Rest/binary>>) -> filter(<<Clean/binary, X>>, Rest).

filter_end(_Dirty, <<">", Rest/binary>>) -> Rest;
filter_end(_Dirty, <<>>) -> false;
filter_end(Dirty, <<X, Rest/binary>>) -> filter_end(<<Dirty/binary, X>>, Rest).

