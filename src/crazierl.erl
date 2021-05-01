-module(crazierl).
-export([start/0, inb/1, inl/1, outb/2, outl/2, map/2, map_addr/1, bcopy_to/3, bcopy_from/3]).
-export([open_interrupt/1]).

-on_load(init/0).

init() ->
	ok = erlang:load_nif("/obj/crazierl_nif", 0).

inb(_Port) -> exit(nif_library_not_loaded).
inl(_Port) -> exit(nif_library_not_loaded).
outb(_Port, _Value) -> exit(nif_library_not_loaded).
outl(_Port, _Value) -> exit(nif_library_not_loaded).

map(_Start, _Length) -> exit(nif_library_not_loaded).
map_addr(_MapResource) -> exit(nif_library_not_loaded).
bcopy_to(_MapResource, _Offset, _Binary) -> exit(nif_library_not_loaded).
bcopy_from(_MapResource, _Offset, _Length) -> exit(nif_library_not_loaded).

% setup console
start() ->
	console:start([
		{comport, start, [16#3f8, "/kern/ioapic/4/0"]},
		{vgakb, start, [16#60, "/kern/ioapic/1/0"]}
	]),
	pci:start(),
	pci:attach(virtio_net, []),
	pci:attach(rtl_8168, []),
	try
		virtio_net ! {ping, self()},
		receive
			pong -> ok
		end
	catch _:_ -> ok
	end,
	try
		rtl_8168 ! {ping, self()},
		receive
			pong -> ok
		end
	catch _:_ -> ok
	end,
	example_host:start(),
	dhcpc:go().

open_interrupt(Irq) ->
        Path = io_lib:format("/kern/irq/~B", [Irq]),
        {ok, Socket} = gen_udp:open(0, [
                {ifaddr, {local, Path}},
                {active, true}
        ]),
        {ok, {local, ActualPath}} = inet:sockname(Socket),
        {ok, [ActualIrq], []} = io_lib:fread("/kern/irq/~d", binary_to_list(ActualPath)),
        {Socket, ActualIrq}.
