-module(crazierl).
-export([start/0, inb/1, outb/2, map/2, bcopy_to/2, bcopy_from/2]).

-on_load(init/0).

init() ->
	ok = erlang:load_nif("/obj/crazierl_nif", 0).

inb(_Port) -> exit(nif_library_not_loaded).
outb(_Port, _Value) -> exit(nif_library_not_loaded).

map(_Start, _Length) -> exit(nif_library_not_loaded).
bcopy_to(_Start, _Binary) -> exit(nif_library_not_loaded).
bcopy_from(_Start, _Length) -> exit(nif_library_not_loaded).

% setup console
start() ->
	console:start([
		{comport, start, [16#3f8, "/kern/pic1/4"]},
		{vgakb, start, [16#60, "/kern/pic1/1"]}
	]).
