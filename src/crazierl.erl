-module(crazierl).
-export([inb/1, outb/2, read_com/0]).

-on_load(init/0).

init() ->
	ok = erlang:load_nif("/obj/crazierl_nif", 0).

inb(_Port) -> exit(nif_library_not_loaded).
outb(_Port, _Value) -> exit(nif_library_not_loaded).


read_com() ->
	case inb(16#3f8 + 5) band 1 of
		1 ->
			In = inb(16#3f8),
			io:put_chars([In]),
			read_com();
		0 ->
			io:put_chars([$\n]),
			ok
	end.
	