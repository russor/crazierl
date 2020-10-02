-module (virtio_net).

-include ("pci.hrl").
-export([check/2, attach/2]).
-record (virtio_pci_cap, {bar, offset, length, data}).
-record (virtq, {map, avail_idx, used, used_idx, notify}).

% TODO split virtio and net bits

% TODO generate these from structs
-define (DEVICE_FEATURE_SELECT, 0).
-define (DEVICE_FEATURE, 4).
-define (DRIVER_FEATURE_SELECT, 8).
-define (DRIVER_FEATURE, 12).
-define (DEVICE_STATUS, 20).
-define (QUEUE_SELECT, 22).
-define (QUEUE_SIZE, 24).
-define (QUEUE_MSIX_VECTOR, 26).
-define (QUEUE_ENABLE, 28).
-define (QUEUE_NOTIFY_OFF, 30).
-define (QUEUE_DESC, 32).
-define (QUEUE_DRIVER, 40).
-define (QUEUE_DEVICE, 48).

-define (VIRTQ_LEN, (1 bsl 2)). % must be power of two
-define (VIRTQ_DESC_SIZE, 16).
-define (VIRTQ_DESC_TABLE_LEN, (?VIRTQ_DESC_SIZE * ?VIRTQ_LEN)).
-define (VIRTQ_AVAIL_SIZE, 2).
-define (VIRTQ_AVAIL_RING_LEN, (6 + (?VIRTQ_AVAIL_SIZE *?VIRTQ_LEN))).
-define (VIRTQ_AVAIL_USED_PADDING, 2). % will always need to pad 2 bytes for alignment
-define (VIRTQ_USED_SIZE, 8).
-define (VIRTQ_USED_RING_LEN, (6 + (?VIRTQ_USED_SIZE * ?VIRTQ_LEN))).
-define (VIRTQ_BUFFER_SIZE, (1514 + 12)). % 1514 max packet + 12 byte header
-define (VIRTQ_BUFFER_LEN, (?VIRTQ_BUFFER_SIZE * ?VIRTQ_LEN)).

-define (VIRTQ_AVAIL_START, ?VIRTQ_DESC_TABLE_LEN).
-define (VIRTQ_USED_START, (?VIRTQ_DESC_TABLE_LEN + ?VIRTQ_AVAIL_RING_LEN
                         + ?VIRTQ_AVAIL_USED_PADDING)).
-define (VIRTQ_BUFFER_START, (?VIRTQ_DESC_TABLE_LEN + ?VIRTQ_AVAIL_RING_LEN
                         + ?VIRTQ_AVAIL_USED_PADDING + ?VIRTQ_USED_RING_LEN)).
-define (VIRTQ_TOTAL_LEN, (?VIRTQ_BUFFER_START + ?VIRTQ_BUFFER_LEN)).


check(#pci_device{common = #pci_common{vendor = 16#1AF4, device_id = 16#1000}}, _Args) -> true;
check(#pci_device{common = #pci_common{vendor = 16#1AF4, device_id = 16#1041}}, _Args) -> true.

attach(Device, _Args) ->
	register(?MODULE, self()),
	Common = Device#pci_device.common,
	Capabilities = parse_capabilities(Common#pci_common.capabilities, #{}),
	{ok, CommonMap} = map_structure(common_cfg, Device, Capabilities),
	crazierl:bcopy_to(CommonMap, ?DEVICE_STATUS, <<0>>), % reset device
	<<0>> = crazierl:bcopy_from(CommonMap, ?DEVICE_STATUS, 1), % confirm
	crazierl:bcopy_to(CommonMap, ?DEVICE_STATUS, <<1>>), % OS ACK
	<<1>> = crazierl:bcopy_from(CommonMap, ?DEVICE_STATUS, 1), % confirm
	crazierl:bcopy_to(CommonMap, ?DEVICE_STATUS, <<3>>), % driver ACK

	crazierl:bcopy_to(CommonMap, ?DEVICE_FEATURE_SELECT, <<0:32/little>>),
	<<DeviceFeaturesLo:32/little>> = crazierl:bcopy_from(CommonMap, ?DEVICE_FEATURE, 4),
	crazierl:bcopy_to(CommonMap, ?DEVICE_FEATURE_SELECT, <<1:32/little>>),
	<<DeviceFeaturesHi:32/little>> = crazierl:bcopy_from(CommonMap, ?DEVICE_FEATURE, 4),
	DeviceFeatures = parse_features(<<DeviceFeaturesHi:32, DeviceFeaturesLo:32>>),
	%io:format("~w~n", [DeviceFeatures]),
	true = maps:is_key(virtio_version_1, DeviceFeatures),
	true = maps:is_key(mac, DeviceFeatures),
	DriverFeatures = make_features([virtio_version_1, mac]),
	<<DriverFeaturesLo:4/binary, DriverFeaturesHi:4/binary>> = <<DriverFeatures:64/little>>,
	crazierl:bcopy_to(CommonMap, ?DRIVER_FEATURE_SELECT, <<0:32/little>>),
	crazierl:bcopy_to(CommonMap, ?DRIVER_FEATURE, DriverFeaturesLo),
	crazierl:bcopy_to(CommonMap, ?DRIVER_FEATURE_SELECT, <<1:32/little>>),
	crazierl:bcopy_to(CommonMap, ?DRIVER_FEATURE, DriverFeaturesHi),

	<<3>> = crazierl:bcopy_from(CommonMap, ?DEVICE_STATUS, 1), % confirm
	crazierl:bcopy_to(CommonMap, ?DEVICE_STATUS, <<11>>), % features_OK
	{ok, DeviceMap} = map_structure(device_cfg, Device, Capabilities),
	<<MacAddr:48>> = crazierl:bcopy_from(DeviceMap, 0, 6),

	{ok, NotifyMap} = map_structure(notify_cfg, Device, Capabilities),

	<<NotifyOffsetMult:32/little>> = (maps:get(notify_cfg, Capabilities))#virtio_pci_cap.data,
	RxQ = setup_virtq(CommonMap, read, 0, NotifyOffsetMult, NotifyMap),
	TxQ = setup_virtq(CommonMap, write, 1, NotifyOffsetMult, NotifyMap),

	<<11>> = crazierl:bcopy_from(CommonMap, ?DEVICE_STATUS, 1), % confirm
	crazierl:bcopy_to(CommonMap, ?DEVICE_STATUS, <<15>>), % features_OK
	<<15>> = crazierl:bcopy_from(CommonMap, ?DEVICE_STATUS, 1), % confirm
	arp:register(self(), {10,0,2,15}, MacAddr),
	RxQ2 = offer_desc(RxQ, {0, ?VIRTQ_LEN - 1}),
	loop(Device, MacAddr, RxQ2, TxQ).

parse_capabilities([{vendor_specific, <<RawType:8, Bar:8, _:3/binary, Offset:32/little, Length:32/little, Data/binary>>}|T], Map) ->
	Type = virtio_cfg_type(RawType),
	case maps:is_key(Type, Map) of
		true -> parse_capabilities(T, Map);
		false -> parse_capabilities(T, Map#{Type => #virtio_pci_cap{bar = Bar, offset = Offset, length = Length, data = Data}})
	end;
parse_capabilities([_H|T], Map) -> parse_capabilities(T, Map);
parse_capabilities([], Map) -> Map.

map_structure(Key, #pci_device{bars = Bars}, Caps) ->
	Cap = maps:get(Key, Caps),
	Bar = element(Cap#virtio_pci_cap.bar + 1, Bars),
	true = (Bar#pci_mem_bar.size >= (Cap#virtio_pci_cap.offset + Cap#virtio_pci_cap.length)),
	crazierl:map(Bar#pci_mem_bar.base + Cap#virtio_pci_cap.offset, Cap#virtio_pci_cap.length).

loop(Device, MacAddr, RxQ, TxQ) ->
	TxQ1 = receive
		{'$gen_call', From, {send, {DestMac, EtherType}, Payload}} ->
			Packet = <<DestMac:48, MacAddr:48, EtherType:16, Payload/binary>>,
			io:format("sending ... ~w~n", [Packet]),
			gen:reply(From, ok),
			add_to_queue(TxQ, Packet)
	after 1000 ->
		TxQ
	end,
	NewRx = check_queue(RxQ, read),
	TxQ2 = check_queue(TxQ1, write),
	loop(Device, MacAddr, NewRx, TxQ2).


virtio_cfg_type(1) -> common_cfg;
virtio_cfg_type(2) -> notify_cfg;
virtio_cfg_type(3) -> isr_cfg;
virtio_cfg_type(4) -> device_cfg;
virtio_cfg_type(5) -> pci_cfg;
virtio_cfg_type(O) -> {reserved, O}.

parse_features(Binary) -> parse_features(Binary, #{}).

parse_features(<<1:1, Rest/bitstring>>, Acc) ->
	parse_features(Rest, Acc#{feature_flag(bit_size(Rest)) => true});
parse_features(<<0:1, Rest/bitstring>>, Acc) -> parse_features(Rest, Acc);
parse_features(<<>>, Acc) -> Acc.

make_features(List) -> make_features(List, 0).

make_features([H | T], Acc) ->
	Bit = feature_flag(H),
	make_features(T, Acc bor (1 bsl Bit));
make_features([], Acc) -> Acc.

feature_flag(0) -> checksum;
feature_flag(1) -> guest_checksum;
feature_flag(2) -> control_guest_offloads;
feature_flag(3) -> mtu;
feature_flag(5) -> mac;
feature_flag(6) -> legacy_gso;
feature_flag(7) -> guest_tso4;
feature_flag(8) -> guest_tso6;
feature_flag(9) -> guest_ecn;
feature_flag(10) -> guest_ufo;
feature_flag(11) -> host_tso4;
feature_flag(12) -> host_tso6;
feature_flag(13) -> host_ecn;
feature_flag(14) -> host_ufo;
feature_flag(15) -> merge_rxbuf;
feature_flag(16) -> config_status;
feature_flag(17) -> control_vq;
feature_flag(18) -> control_rx;
feature_flag(19) -> control_vlan;
feature_flag(21) -> guest_announce;
feature_flag(22) -> multi_queue;
feature_flag(23) -> ctrl_mac_addr;
feature_flag(28) -> virtio_ring_indirect_desc;
feature_flag(29) -> virtio_ring_event_idx;
feature_flag(32) -> virtio_version_1;
feature_flag(33) -> virtio_access_platform;
feature_flag(34) -> virito_ring_packed;
feature_flag(35) -> virtio_in_order;
feature_flag(36) -> virtio_order_platform;
feature_flag(37) -> virtio_sr_iov;
feature_flag(38) -> virtio_notification_data;
feature_flag(41) -> legacy_guest_rsc4;
feature_flag(52) -> legacy_guest_rsc6;
feature_flag(61) -> rsc_ext;
feature_flag(62) -> standby;
feature_flag(N) when is_integer(N) -> {unknown, N};

feature_flag(checksum) -> 0;
feature_flag(guest_checksum) -> 1;
feature_flag(control_guest_offloads) -> 2;
feature_flag(mtu) -> 3;
feature_flag(mac) -> 5;
feature_flag(legacy_gso) -> 6;
feature_flag(guest_tso4) -> 7;
feature_flag(guest_tso6) -> 8;
feature_flag(guest_ecn) -> 9;
feature_flag(guest_ufo) -> 10;
feature_flag(host_tso4) -> 11;
feature_flag(host_tso6) -> 12;
feature_flag(host_ecn) -> 13;
feature_flag(host_ufo) -> 14;
feature_flag(merge_rxbuf) -> 15;
feature_flag(config_status) -> 16;
feature_flag(control_vq) -> 17;
feature_flag(control_rx) -> 18;
feature_flag(control_vlan) -> 19;
feature_flag(guest_announce) -> 21;
feature_flag(multi_queue) -> 22;
feature_flag(ctrl_mac_addr) -> 23;
feature_flag(virtio_ring_indirect_desc) -> 28;
feature_flag(virtio_ring_event_idx) -> 29;
feature_flag(virtio_version_1) -> 32;
feature_flag(virtio_access_platform) -> 33;
feature_flag(virito_ring_packed) -> 34;
feature_flag(virtio_in_order) -> 35;
feature_flag(virtio_order_platform) -> 36;
feature_flag(virtio_sr_iov) -> 37;
feature_flag(virtio_notification_data) -> 38;
feature_flag(legacy_guest_rsc4) -> 41;
feature_flag(legacy_guest_rsc6) -> 52;
feature_flag(rsc_ext) -> 61;
feature_flag(standby) -> 62.

setup_virtq(CommonMap, Type, QueueNum, NotifyMult, NotifyMap) ->
	{ok, Map} = crazierl:map(0, ?VIRTQ_TOTAL_LEN),
	{_Virtual, Physical} = crazierl:map_addr(Map),
	Flags = case Type of
		read -> 2;
		write -> 0
	end,
	DescriptorTable = virtq_descriptors(Physical + ?VIRTQ_BUFFER_START, Flags, 0, <<>>),
	crazierl:bcopy_to(Map, 0, <<DescriptorTable/binary, 1:16/little>>), % available ring has notifications disabled and nothing available
	Used = case Type of
		read -> ignore;
		write -> [{0, ?VIRTQ_LEN - 1}]
	end,
	crazierl:bcopy_to(CommonMap, ?QUEUE_SELECT, <<QueueNum:16/little>>),
	crazierl:bcopy_to(CommonMap, ?QUEUE_SIZE, <<?VIRTQ_LEN:16/little>>),
	crazierl:bcopy_to(CommonMap, ?QUEUE_DESC, <<Physical:64/little>>),
	crazierl:bcopy_to(CommonMap, ?QUEUE_DRIVER, <<(Physical + ?VIRTQ_AVAIL_START):64/little>>),
	crazierl:bcopy_to(CommonMap, ?QUEUE_DEVICE, <<(Physical + ?VIRTQ_USED_START):64/little>>),
	crazierl:bcopy_to(CommonMap, ?QUEUE_ENABLE, <<1:16/little>>),
	<<NotifyOffset:16/little>> = crazierl:bcopy_from(CommonMap, ?QUEUE_NOTIFY_OFF, 2),
	#virtq{map = Map, avail_idx = 0, used = Used, used_idx = 0, notify= {NotifyMap, NotifyOffset * NotifyMult, QueueNum}}.

virtq_descriptors(_, _, ?VIRTQ_LEN, Acc) -> Acc;
virtq_descriptors(Physical, Flags, Index, Acc) ->
	Acc0 = <<Acc/binary, Physical:64/little, ?VIRTQ_BUFFER_SIZE:32/little, Flags:16/little, 0:16>>,
	virtq_descriptors(Physical + ?VIRTQ_BUFFER_SIZE, Flags, Index + 1, Acc0).

offer_desc(Queue, {N, N}) -> offer_desc(Queue, N);
offer_desc(Queue, {N, T}) ->
	Out = offer_desc_impl(Queue, N),
	offer_desc(Out, {N + 1, T});
offer_desc(Queue, N) ->
	Out = offer_desc_impl(Queue, N),
	write_avail_idx(Out),
	Out.

offer_desc_impl(Queue = #virtq{map = Map, avail_idx = Idx}, N) ->
	crazierl:bcopy_to(Map, ?VIRTQ_AVAIL_START + 4 + (2 * (Idx band (?VIRTQ_LEN - 1))), <<N:16/little>>),
	Queue#virtq{avail_idx = Idx + 1}.

write_avail_idx(#virtq{map = Map, avail_idx = Idx, notify = {NotifyMap, Offset, QueueNum}}) ->
	crazierl:bcopy_to(Map, ?VIRTQ_AVAIL_START + 2, <<Idx:16/little>>),
	<<Flags:16/little>> = crazierl:bcopy_from(Map, ?VIRTQ_USED_START, 2),
	case Flags band 1 of
		1 -> ok;
		0 -> crazierl:bcopy_to(NotifyMap, Offset, <<QueueNum:16/little>>)
	end.

add_to_queue(Queue = #virtq{used = []}, Packet) ->
	io:format("virtq full, dropping ~w~n", [Packet]),
	Queue;
add_to_queue(Queue = #virtq{map = Map, used = Used}, Packet) ->
	{Descriptor, NewUsed} = get_descriptor(Used),
	io:format("got ~p~n", [{Descriptor, NewUsed}]),
	crazierl:bcopy_to(Map, ?VIRTQ_BUFFER_START + Descriptor * ?VIRTQ_BUFFER_SIZE,
		<<0, 0, 0:16, 0:16, 0:16, 0:16, 0:16, Packet/binary>>),
	crazierl:bcopy_to(Map, Descriptor * ?VIRTQ_DESC_SIZE + 8,
		<<(size(Packet) + 12):32/little>>),
	offer_desc(Queue#virtq{used = NewUsed}, Descriptor).

get_descriptor([{A, B} | T]) when B == A + 1 -> {A, [B | T]};
get_descriptor([{A, B} | T]) -> {A, [{A + 1, B} | T]};
get_descriptor([H | T]) -> {H, T}.
add_to_used([B|T], H) when B == H + 1 -> [{H, B} | T];
add_to_used([{A, B}|T], H) when A == H + 1 -> [{H, B} | T];
add_to_used(T, H) -> [H | T].

check_queue(Queue = #virtq{map = Map, used_idx = Idx}, Type) ->
	case crazierl:bcopy_from(Map, ?VIRTQ_USED_START + 2, 2) of
		<<Idx:16/little>> -> Queue;
		<<NewIdx:16/little>> ->
			process_packet(Queue, Type, NewIdx)
	end.

process_packet(Queue = #virtq{used_idx = Idx}, _, Idx) -> Queue;
process_packet(Queue = #virtq{map = Map, used_idx = Idx}, read, NewIndex) ->
	io:format("reading used ring item (read) ~B~n", [Idx]),
	<<Id:32/little, Len:32/little>> = crazierl:bcopy_from(Map, ?VIRTQ_USED_START + 4 + (8 * (Idx band (?VIRTQ_LEN -1 ))), 8),
	io:format("descriptor ~B, length ~B~n", [Id, Len]),
	<<_Flags:8, _GsoType:8, _HdrLen:16/little, _GsoSize:16/little,
	  _CsumStart:16/little, _CSumOffset:16/little, _NumBuffers:16/little,
	  Data/binary>> = crazierl:bcopy_from(Map, ?VIRTQ_BUFFER_START + Id * ?VIRTQ_BUFFER_SIZE, Len),
	%io:format("Flags ~.16B, GSO ~.16B, HDR Len ~B, GSO size ~B, Csum ~B @ ~B, Buffs ~B~n",
	%	[Flags, GsoType, HdrLen, GsoSize, CsumStart, CSumOffset, NumBuffers]),

	<<DestMac:48, SourceMac:48, EtherType:16, Payload/binary>> = Data,
	case EtherType of
		16#0806 -> arp:process(?MODULE, Payload);
		16#0800 -> ip:process(?MODULE, Payload);
		_ ->
			io:format("~12.16.0B > ~12.16.0B, EtherType ~4.16.0B, ~w~n", [SourceMac, DestMac, EtherType, Payload])
	end,
	offer_desc(Queue, Id),
	process_packet(Queue#virtq{used_idx = (Idx + 1) band ((1 bsl 16) - 1)}, read, NewIndex);

process_packet(Queue = #virtq{map = Map, used = Used, used_idx = Idx}, write, NewIndex) ->
	io:format("reading used ring item (write) ~B~n", [Idx]),
	<<Id:32/little, Len:32/little>> = crazierl:bcopy_from(Map, ?VIRTQ_USED_START + 4 + (8 * (Idx band (?VIRTQ_LEN -1 ))), 8),
	io:format("descriptor ~B, length ~B~n", [Id, Len]),
	NewUsed = add_to_used(Used, Id),
	process_packet(Queue#virtq{used = NewUsed, used_idx = (Idx + 1) band ((1 bsl 16) - 1)}, write, NewIndex).
