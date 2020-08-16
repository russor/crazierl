-module(pci).

-export([start/0]).
-include("pci.hrl").

pci_order(#pci_device{common = A}, B) -> pci_order(A, B);
pci_order(#pci_bridge{common = A}, B) -> pci_order(A, B);
pci_order(A, #pci_device{common = B}) -> pci_order(A, B);
pci_order(A, #pci_bridge{common = B}) -> pci_order(A, B);
pci_order(A, B) -> A =< B.

start() ->
	AllDevices = scan_bus(0, 0, 0, []),
	Sorted = lists:sort(fun pci_order/2, AllDevices),
	print_pci(Sorted).

scan_bus(_, 32, _, Acc) -> Acc; % only 32 devices per Bus
scan_bus(Bus, Device, Function, Acc) ->
	{PCIFunction, HasMultiFunction} = probe_device(Bus, Device, Function, <<>>),
	Acc1 = case PCIFunction of
		none -> Acc;
		#pci_bridge{secondary_bus = NextBus} ->
			scan_bus(NextBus, 0, 0, [PCIFunction | Acc]);
		_ -> [PCIFunction | Acc]
	end,
	Acc2 = case HasMultiFunction of
		1 when Function == 0-> 
			lists:foldl(fun (N, AccX) ->
				scan_bus(Bus, Device, N, AccX) end, Acc1, lists:seq(1, 7));
		0 -> Acc1
	end,
	case Function of
		0 -> scan_bus(Bus, Device + 1, 0, Acc2);
		_ -> Acc2
	end.

probe_device(_Bus, _Device, _Function, <<16#FFFFFFFF:32/little>>) ->
	{none, 0};
probe_device(Bus, Device, Function, Config) when size(Config) == 256 ->
	<<Vendor:16/little, DeviceId:16/little,
	  % Command
	  _:1, _ParityErrorResponse:1, _VGAPaletteSnoop:1, _MemWriteInvalEnable:1, _SpecialCycles:1, _BusMaster:1, _MemSpace:1, _IOSpace:1,
	  _:4, _InterruptDisable:1, _FastBack2BackEnabled:1, _SerrEnable:1, _:1,
	  % Status
	  _FastBack2BackCapable:1, _:1, _Sixty6MHZ:1, HasCapabilitiesList:1, _InterruptStatus:1, _:3,
	  _ParityError:1, _SystemError:1, _RecvMasterAbort:1, _RecvTargetAbort:1, _SentTargetAbort:1, _DevSelTiming:2, _MasterDataParityError:1,
	  RevisionId:8, ProgIf:8, SubClass:8, Class:8,
	  _CacheLineSize:8, _LatencyTimer:8, MultiFunction:1, HeaderType:7, _BIST:8,
	  TypeSpecific/binary>> = Config,
	%io:format("~2000p~n", [TypeSpecific]),
	CapList = case HasCapabilitiesList of
		0 -> [];
		1 -> tba
	end,
	PCICommon = #pci_common{bus = Bus, device = Device, function = Function, vendor = Vendor, device_id = DeviceId,
				class = Class, sub_class = SubClass, revision = RevisionId, prog_if = ProgIf,
				capabilities = CapList},
	Return = case HeaderType of
		0 -> <<BAR0:4/binary, BAR1:4/binary, BAR2:4/binary, BAR3:4/binary, BAR4:4/binary, BAR5:4/binary,
			  _CardBusCIS:4/binary,
			  ChipVendor:16/little, ChipDeviceId:16/little,
			  _ExpansionRomBase:32/little,
			  _Capabilities:8, _:24,
			  _:32,
			  InterruptLine:8, InterruptPIN:8, _MinGrant:8, _MaxLatency:8, _/binary>> = TypeSpecific,
			#pci_device{common = PCICommon, chip_vendor = ChipVendor, chip_device_id = ChipDeviceId,
			            interrupt_line = InterruptLine, interrupt_pin = InterruptPIN,
			            bar0 = probe_bar(PCICommon, BAR0, 16#10),
			            bar1 = probe_bar(PCICommon, BAR1, 16#14),
			            bar2 = probe_bar(PCICommon, BAR2, 16#18),
			            bar3 = probe_bar(PCICommon, BAR3, 16#1C),
			            bar4 = probe_bar(PCICommon, BAR4, 16#20),
			            bar5 = probe_bar(PCICommon, BAR5, 16#24)
			            };
		1 -> <<BAR0:4/binary, BAR1:4/binary,
		       _PrimaryBus:8, SecondaryBus:8, _Rest>> = TypeSpecific,
			#pci_bridge{common = PCICommon,
			            bar0 = probe_bar(PCICommon, BAR0, 16#10),
			            bar1 = probe_bar(PCICommon, BAR1, 16#14),
			            secondary_bus = SecondaryBus}
	end,
	{Return, MultiFunction};
probe_device(Bus, Device, Function, Bin) ->
	NextWord = pciConfigReadWord(Bus, Device, Function, size(Bin)),
	probe_device(Bus, Device, Function, <<Bin/binary, NextWord/binary>>).

probe_bar(_, <<0:32>>, _) -> none;
probe_bar(#pci_common{bus = Bus, device = Device, function = Function}, <<BaseLS:4, Prefetch:1, Type:2, 0:1, BaseMS:24/little>> = BAR, Offset) ->
	<<Base:32>> = <<BaseMS:24, BaseLS:4, 0:4>>,
	pciConfigWriteWord(Bus, Device, Function, Offset, <<16#F:4, Prefetch:1, Type:2, 0:1, 16#FFFFFF:24>>),
	<<ReflectLS:4, _:4, ReflectMS:24/little>> = pciConfigReadWord(Bus, Device, Function, Offset),
	<<Reflect:32/signed>> = <<ReflectMS:24, ReflectLS:4, 0:4>>,
	pciConfigWriteWord(Bus, Device, Function, Offset, BAR),
	#pci_mem_bar{base = Base, size = -Reflect, prefetch = (Prefetch == 1), type = Type};

probe_bar(#pci_common{bus = Bus, device = Device, function = Function}, <<BaseLS:6, Reserved:1, 1:1, BaseMS:24/little>> = BAR, Offset) ->
	<<Base:32>> = <<BaseMS:24, BaseLS:6, 0:2>>,
	pciConfigWriteWord(Bus, Device, Function, Offset, <<16#FF:6, Reserved:1, 1:1, 16#FFFFFF:24>>),
	<<ReflectLS:6, _:2, ReflectMS:24/little>> = pciConfigReadWord(Bus, Device, Function, Offset),
	<<Reflect:32/signed>> = <<ReflectMS:24, ReflectLS:6, 0:2>>,
	pciConfigWriteWord(Bus, Device, Function, Offset, BAR),
	#pci_io_bar{base = Base, size = -Reflect}.


pciFunctionAddress(Bus, Device, Function, Offset) ->
	<<Address:32>> = <<1:1, 0:7, Bus:8, Device:5, Function:3, Offset:8>>,
	Address.

pciConfigReadWord(Bus, Device, Function, Offset) when Offset band 3 == 0 ->
	Address = pciFunctionAddress(Bus, Device, Function, Offset),
	crazierl:outl(16#CF8, Address),
	Out = crazierl:inl(16#CFC),
	<<Out:32/little>>.
pciConfigWriteWord(Bus, Device, Function, Offset, Value) when is_binary(Value) ->
	<<V:32/little>> = Value,
	pciConfigWriteWord(Bus, Device, Function, Offset, V);
pciConfigWriteWord(Bus, Device, Function, Offset, Value) when Offset band 3 == 0 ->
	Address = pciFunctionAddress(Bus, Device, Function, Offset),
	crazierl:outl(16#CF8, Address),
	crazierl:outl(16#CFC, Value).
	
print_pci([]) -> ok;
print_pci([#pci_device{common = C} = D | Tail]) ->
	io:format("pci ~2B:~2B:~B class=0x~4.16.0B~4.16.0B card=0x~4.16.0B~4.16.0B chip=0x~4.16.0B~4.16.0B rev=0x~2.16.0B~n",
	io:for
		[C#pci_common.bus, C#pci_common.device, C#pci_common.function,
		 C#pci_common.class, C#pci_common.sub_class,
		 C#pci_common.device_id, C#pci_common.vendor,
		 D#pci_device.chip_device_id, D#pci_device.chip_vendor,
		 C#pci_common.revision
		]),
	print_pci(Tail).

