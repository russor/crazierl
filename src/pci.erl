-module(pci).
-export([start/0]).


start() -> 
	Vendor = pciConfigReadWord(0, 0, 0, 0),
	io:format("Vendor ~.16B~n", [Vendor]).

pciConfigReadWord(Bus, Device, Function, Offset) ->
	<<Address:32>> = <<1:1, 0:7, Bus:8, Device:5, Function:3, Offset:8>>,
	io:format("Address is ~.16B~n", [Address]),
	crazierl:outl(16#CF8, Address),
	Out = crazierl:inl(16#CFC),
	if
		Offset band 2 == 2 -> Out bsr 16;
		true -> Out band 16#FFFF
	end.
	
		


