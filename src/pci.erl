-module(pci).
-export([start/0]).


start() ->
	scan_bus(0, 0).

scan_bus(_, 32) -> ok; % only 32 devices per Bus
scan_bus(Bus, Device) ->
	<<DeviceId:16, Vendor:16>> = pciConfigReadWord(Bus, Device, 0, 0),
	probe_device(Bus, Device, 0, DeviceId, Vendor),
	scan_bus(Bus, Device + 1).


probe_device(_Bus, _Device, _Function, _DeviceId, 16#FFFF) -> ok;
probe_device(Bus, Device, Function, DeviceId, Vendor) ->
	<<_BIST:8, MultiFunction:1, HeaderType:7, _LatencyTimer:8, _CacheLineSize:8>> = pciConfigReadWord(Bus, Device, Function, 16#c),
	io:format("pci ~B:~B:~B ~.16B:~.16B Type ~B~n", [Bus, Device, Function, Vendor, DeviceId, HeaderType]),
	case {MultiFunction, Function} of
		{1, 0} ->
			lists:foreach(fun(N) ->
				<<DID:16, Ven:16>> = pciConfigReadWord(Bus, Device, N, 0),
				probe_device(Bus, Device, N, DID, Ven)
			end, lists:seq(1, 7));
		_ -> ok
	end.

pciConfigReadWord(Bus, Device, Function, Offset) when Offset band 3 == 0 ->
	<<Address:32>> = <<1:1, 0:7, Bus:8, Device:5, Function:3, Offset:8>>,
	crazierl:outl(16#CF8, Address),
	Out = crazierl:inl(16#CFC),
	<<Out:32>>.
	
		


