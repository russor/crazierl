-module (ne2k_pci).

-include ("pci.hrl").
-export([check/2, attach/2]).

-define (RESET_REG, 16#1f).
-define (ISR, 16#07).

check(#pci_device{common = #pci_common{vendor = 16#10EC, device_id = 16#8029}}, _Args) -> true.

attach(Device, _Args) ->
	register(ethernet_sender, self()),
	#pci_io_bar{size = 256, base = Base} = element(1, Device#pci_device.bars),
	
	reset(Base).

reset(Base) -> 
	Val = crazierl:inb(Base + ?RESET_REG),
	crazierl:outb(Base + ?RESET_REG, Val),
	reset_loop(Base).
	
reset_loop(Base) ->
	case crazierl:inb(Base + ?ISR) band 16#80 of
		16#80 -> ok;
		0 -> reset_loop(Base)
	end.
	