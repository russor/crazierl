-module(pci).

-export([start/0, list/0, attach/2, enable_msix/3, map/3]).
-include("pci.hrl").
-behavior(gen_server).

-export ([init/1, handle_cast/2, handle_call/3]).


-record(state, {
	devices
}).

pci_order(#pci_device{common = A}, B) -> pci_order(A, B);
pci_order(#pci_bridge{common = A}, B) -> pci_order(A, B);
pci_order(A, #pci_device{common = B}) -> pci_order(A, B);
pci_order(A, #pci_bridge{common = B}) -> pci_order(A, B);
pci_order(A, B) -> A =< B.

start() ->
	case gen_server:start({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> Pid;
		{error, {already_started, Pid}} -> Pid;
		Other -> Other
	end.

init ([]) ->
	process_flag(trap_exit, true),
	AllDevices = scan_bus(0, 0, 0, []),
	Sorted = lists:sort(fun pci_order/2, AllDevices),
	{ok, #state{devices = Sorted}}.

handle_cast(_, State) -> State.

handle_call({attach, Module, Args}, _From, State) ->
	NewDevices =
	lists:map(fun (Device) -> attach_device_impl(Device, Module, Args) end, State#state.devices),
	{reply, ok, State#state{devices = NewDevices}};

handle_call({enable_msix, #pci_common{msix_map = Map, capabilities = Capabilities} = Common, Vector, IRQ}, _From, State) ->
	Reply = case get_msix(Capabilities) of
		false -> {error, no_msix};
		#pci_msi_x{size = S} when Vector >= S -> {error, invalid_vector};
		#pci_msi_x{offset = Offset} ->
			% FIXME: x86 specific here
			DestAPIC = 0,
			<<AddressLo:32>> = <<16#FEE:12, DestAPIC:8, 0:8, 0:1, 0:1, 0:2>>,
			AddressHi = 0,
			Data = IRQ bor 16#100,
			% FIXME: need MSI settings for other platforms
			crazierl:bcopy_to(Map, Vector * 16 + 0, <<AddressLo:32/little>>),
			crazierl:bcopy_to(Map, Vector * 16 + 4, <<AddressHi:32/little>>),
			crazierl:bcopy_to(Map, Vector * 16 + 8, <<Data:32/little>>),
			crazierl:bcopy_to(Map, Vector * 16 + 12,<<0:32/little>>), % not masked
			<<Capability:8, Next:8, Control:16/little>> = pciConfigReadWord(Common, Offset),
			Enabled = Control bor 16#8000,
			pciConfigWriteWord(Common, Offset, <<Capability:8, Next:8, Enabled:16/little>>),
			% set device to be bus-master, so it can actually write the MSI-X interrupt
			<<Command:16/little, Status:16/little>> = pciConfigReadWord(Common, 4),
			case Command band 4 of
				0 ->
					NewCommand = Command bor 4,
					pciConfigWriteWord(Common, 4, <<NewCommand:16/little, Status:16/little>>);
				4 -> ok
			end
	end,
	{reply, Reply, State};

handle_call(list, _From, State) ->
	{reply, State#state.devices, State}.

attach_device_impl(#pci_device{common = #pci_common {pid = Pid}} = Device, _, _) when is_pid(Pid) -> Device;
attach_device_impl(#pci_bridge{common = #pci_common {pid = Pid}} = Bridge, _, _) when is_pid(Pid) -> Bridge;
attach_device_impl(Device, Module, Args) when is_record(Device, pci_device) ->
	case catch Module:check(Device, Args) of
		true ->
			Pid = spawn_link(Module, attach, [Device, Args]),
			Device#pci_device{common = (Device#pci_device.common)#pci_common{driver = Module, pid = Pid}};
		_ -> Device
	end;
attach_device_impl(Bridge, Module, Args) when is_record(Bridge, pci_bridge) ->
	case catch Module:check(Bridge, Args) of
		true ->
			Pid = spawn_link(Module, attach, [Bridge, Args]),
			Bridge#pci_bridge{common = (Bridge#pci_bridge.common)#pci_common{driver = Module, pid = Pid}};
		_ -> Bridge
	end.


list() ->
	Sorted = gen_server:call(?MODULE, list),
	print_pci(Sorted).

attach(Module, Args) ->
	gen_server:call(?MODULE, {attach, Module, Args}, infinity).

enable_msix(Device, Vector, IRQ) when Vector >= 0 ->
	gen_server:call(?MODULE, {enable_msix, Device, Vector, IRQ}, infinity).

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
		_ -> Acc1
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
		1 ->
			<<CapPtr:6, _:2>> = binary:part(Config, {16#34, 1}),
			probe_capabilities(Config, CapPtr bsl 2, [])
	end,
	PCICommon = #pci_common{bus = Bus, device = Device, function = Function, vendor = Vendor, device_id = DeviceId,
				class = Class, sub_class = SubClass, revision = RevisionId, prog_if = ProgIf,
				capabilities = CapList},
	% FIXME: disable io/mem addressing, so probing bars is safe!
	% maybe this should be done in the driver, devices involved in console (vga, serial, etc)
	% don't get disabled, or get disabled carefully
	Return = case HeaderType of
		0 -> <<BAR0:4/binary, BAR1:4/binary, BAR2:4/binary, BAR3:4/binary, BAR4:4/binary, BAR5:4/binary,
			  _CardBusCIS:4/binary,
			  ChipVendor:16/little, ChipDeviceId:16/little,
			  _ExpansionRomBase:32/little,
			  _Capabilities:8, _:24,
			  _:32,
			  InterruptLine:8, InterruptPIN:8, _MinGrant:8, _MaxLatency:8, _/binary>> = TypeSpecific,
			Bars = probe_bars(PCICommon, 16#10, [BAR0, BAR1, BAR2, BAR3, BAR4, BAR5], []),
			NewCommon = map_msix(PCICommon, Bars),
			#pci_device{common = NewCommon, chip_vendor = ChipVendor, chip_device_id = ChipDeviceId,
			            interrupt_line = InterruptLine, interrupt_pin = InterruptPIN,
			            bars = Bars
			            };
		1 -> <<BAR0:4/binary, BAR1:4/binary,
		       _PrimaryBus:8, SecondaryBus:8, _Rest/binary>> = TypeSpecific,
		        Bars = probe_bars(PCICommon, 16#10, [BAR0, BAR1], []),
		        NewCommon = map_msix(PCICommon, Bars),
			#pci_bridge{common = NewCommon,
			            bars = Bars,
			            secondary_bus = SecondaryBus}
	end,
	% FIXME: restore io/mem addressing
	
	{Return, MultiFunction};
probe_device(Bus, Device, Function, Bin) ->
	NextWord = pciConfigReadWord(Bus, Device, Function, size(Bin)),
	probe_device(Bus, Device, Function, <<Bin/binary, NextWord/binary>>).

% I hope the capabilities list doesn't have a loop!
probe_capabilities(_, _, Acc) when length(Acc) > 255 ->
	lists:reverse([loop | Acc]);
probe_capabilities(Config, Offset, Acc) when Offset band 3 == 0 ->
	<<Type:8, NextMSB:6, _:2>> = binary:part(Config, {Offset, 2}),
	Next = NextMSB bsl 2,
	Cap = parse_capability(Type, Config, Offset),
	case Next of
		0 -> lists:reverse([Cap | Acc]);
		_ -> probe_capabilities(Config, Next, [Cap | Acc])
	end.

parse_capability(9, Config, Offset) ->
	<<Length:8>> = binary:part(Config, {Offset + 2, 1}),
	Data = binary:part(Config, {Offset + 3, Length - 3}),
	{vendor_specific, Data};
parse_capability(16#11, Config, Offset) ->
	<<MessageControl:16/little, TableOffset:32/little, PendingBit:32/little>> = binary:part(Config, {Offset + 2, 10}),
	<<Enable:1, FunctionMask:1, _:3, Size:11>> = <<MessageControl:16>>,
	#pci_msi_x{enabled = Enable, mask = FunctionMask, size = Size + 1,
		table = {(TableOffset band 7), TableOffset band 16#FFFFFFF8},
		pending = {(PendingBit band 7), PendingBit band 16#FFFFFFF8},
		offset = Offset};
parse_capability(Type, _Config, Offset) ->
	{Type, Offset}.

probe_bars(_PCI, _Offset, [], Acc) ->
	list_to_tuple(lists:reverse(Acc));

probe_bars(PCI, Offset, [<<0:32>> | Tail], Acc) ->
	probe_bars(PCI, Offset + 4, Tail, [none|Acc]);

probe_bars(PCI, Offset, [<<BaseLS:4, Prefetch:1, 2:2, 0:1, BaseMS:24/little>> = BARLo, <<BARHi:32/little>> | Tail], Acc) ->
	<<Base:64>> = <<BARHi:32, BaseMS:24, BaseLS:4, 0:4>>,
	pciConfigWriteWord(PCI, Offset, <<16#F:4, Prefetch:1, 2:2, 0:1, 16#FFFFFF:24>>),
	pciConfigWriteWord(PCI, Offset + 4, <<16#FFFFFFFF:32>>),
	<<ReflectLS:4, _:4, ReflectMS:24/little>> = pciConfigReadWord(PCI, Offset),
	<<ReflectHi:32/little>> = pciConfigReadWord(PCI, Offset + 4),
	<<Reflect:64/signed>> = <<ReflectHi:32, ReflectMS:24, ReflectLS:4, 0:4>>,
	pciConfigWriteWord(PCI, Offset, BARLo),
	pciConfigWriteWord(PCI, Offset + 4, BARHi),
	probe_bars(PCI, Offset + 8, Tail, [none, #pci_mem_bar{base = Base, size = -Reflect, prefetch = (Prefetch == 1), type = '64-bit'} | Acc]);

probe_bars(PCI, Offset, [<<BaseLS:4, Prefetch:1, 0:1, Type:1, 0:1, BaseMS:24/little>> = BAR | Tail], Acc) ->
	<<Base:32>> = <<BaseMS:24, BaseLS:4, 0:4>>,
	pciConfigWriteWord(PCI, Offset, <<16#F:4, Prefetch:1, 0:1, Type:1, 0:1, 16#FFFFFF:24>>),
	<<ReflectLS:4, _:4, ReflectMS:24/little>> = pciConfigReadWord(PCI, Offset),
	<<Reflect:32/signed>> = <<ReflectMS:24, ReflectLS:4, 0:4>>,
	pciConfigWriteWord(PCI, Offset, BAR),
	TypeAtom = case Type of
		0 -> '32-bit';
		1 -> '20-bit'
	end,
	probe_bars(PCI, Offset + 4, Tail, [#pci_mem_bar{base = Base, size = -Reflect, prefetch = (Prefetch == 1), type = TypeAtom} | Acc]);

probe_bars(PCI, Offset, [<<BaseLS:6, Reserved:1, 1:1, BaseMS:24/little>> = BAR | Tail], Acc) ->
	<<Base:32>> = <<BaseMS:24, BaseLS:6, 0:2>>,
	pciConfigWriteWord(PCI, Offset, <<16#FF:6, Reserved:1, 1:1, 16#FFFFFF:24>>),
	<<ReflectLS:6, _:2, ReflectMS:24/little>> = pciConfigReadWord(PCI, Offset),
	Reflect = case <<ReflectMS:24, ReflectLS:6, 0:2>> of
		<<0:16, Val:16/signed>> -> Val;
		<<Val:32/signed>> -> Val
	end,
	pciConfigWriteWord(PCI, Offset, BAR),
	probe_bars(PCI, Offset + 4, Tail, [#pci_io_bar{base = Base, size = -Reflect}| Acc]).


pciFunctionAddress(Bus, Device, Function, Offset) ->
	<<Address:32>> = <<1:1, 0:7, Bus:8, Device:5, Function:3, Offset:8>>,
	Address.

pciConfigReadWord(#pci_common{bus = Bus, device = Device, function = Function}, Offset) ->
	pciConfigReadWord(Bus, Device, Function, Offset).
pciConfigReadWord(Bus, Device, Function, Offset) when Offset band 3 == 0 ->
	Address = pciFunctionAddress(Bus, Device, Function, Offset),
	crazierl:outl(16#CF8, Address),
	Out = crazierl:inl(16#CFC),
	<<Out:32/little>>.
pciConfigWriteWord(#pci_common{bus = Bus, device = Device, function = Function}, Offset, Value) ->
	pciConfigWriteWord(Bus, Device, Function, Offset, Value).
pciConfigWriteWord(Bus, Device, Function, Offset, Value) when is_binary(Value) ->
	<<V:32/little>> = Value,
	pciConfigWriteWord(Bus, Device, Function, Offset, V);
pciConfigWriteWord(Bus, Device, Function, Offset, Value) when Offset band 3 == 0 ->
	Address = pciFunctionAddress(Bus, Device, Function, Offset),
	crazierl:outl(16#CF8, Address),
	crazierl:outl(16#CFC, Value).
	
print_pci([]) -> ok;
print_pci([#pci_bridge{common = C} = B | Tail]) ->
	io:format("pci ~2B:~2B:~B class=~4.16.0B~4.16.0B card=~4.16.0B~4.16.0B chip=~4.16.0B~4.16.0B rev=~2.16.0B type=bridge~n",
		[C#pci_common.bus, C#pci_common.device, C#pci_common.function,
		 C#pci_common.class, C#pci_common.sub_class,
		 C#pci_common.device_id, C#pci_common.vendor,
		 0, 0,
%		 B#pci_device.chip_device_id, B#pci_device.chip_vendor,
		 C#pci_common.revision
		]),
	print_driver(C),
	print_bars(B#pci_bridge.bars),
	print_pci(Tail);
print_pci([#pci_device{common = C} = D | Tail]) ->
	io:format("pci ~2B:~2B:~B class=~4.16.0B~4.16.0B card=~4.16.0B~4.16.0B chip=~4.16.0B~4.16.0B rev=~2.16.0B type=device~n",
		[C#pci_common.bus, C#pci_common.device, C#pci_common.function,
		 C#pci_common.class, C#pci_common.sub_class,
		 C#pci_common.device_id, C#pci_common.vendor,
		 D#pci_device.chip_device_id, D#pci_device.chip_vendor,
		 C#pci_common.revision
		]),
	print_driver(C),
	print_bars(D#pci_device.bars),
	print_pci(Tail).

print_driver(#pci_common{driver = undefined}) -> ok;
print_driver(#pci_common{driver = Driver, pid = Pid}) ->
	io:format("    driver ~s, pid ~w~n", [Driver, Pid]).

print_bars(Bars) ->
	print_bars(tuple_to_list(Bars), 0).
print_bars([Head|Tail], Index) ->
	print_bar(Head, Index),
	print_bars(Tail, Index + 1);
print_bars([], _) -> ok.

print_bar(none, _) -> ok;
print_bar(#pci_io_bar{base = Base, size = Size}, N) ->
	io:format("    bar~B = I/O size ~B, base 0x~.16B~n", [N, Size, Base]);
print_bar(#pci_mem_bar{base = Base, size = Size, prefetch = Prefetch, type = Type}, N) ->
	io:format("    bar~B = mem size ~B, base 0x~.16B, prefetch ~p, type ~s~n", [N, Size, Base, Prefetch, Type]).

map_msix(#pci_common{capabilities = Capabilities} = Common, Bars) ->
	case get_msix(Capabilities) of
		#pci_msi_x {size = Size, table = {BarNumber, BarOffset}} ->
			Length = Size * 16,
			Bar = element(BarNumber + 1, Bars),
			{ok, Map} = map(Bar, BarOffset, Length),
			Common#pci_common{msix_map = Map};
		false -> Common
	end.

map(#pci_mem_bar{base = Base, size = Size}, Offset, Length) when Size >= Offset + Length ->
	crazierl:map(Base + Offset, Length);

map(#pci_io_bar{base = Base, size = Size}, Offset, Length) when Size >= Offset + Length ->
	crazierl:map_port(Base + Offset, Length).

get_msix([#pci_msi_x{} = X | _]) -> X;
get_msix([_ | T]) -> get_msix(T);
get_msix([]) -> false.
