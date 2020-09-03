-module (virtio_net).

-include ("pci.hrl").
-export([check/2, attach/2]).
-record (virtio_pci_cap, {bar, offset, length}).


check(#pci_device{common = #pci_common{vendor = 16#1AF4, device_id = 16#1000}}, _Args) -> true;
check(#pci_device{common = #pci_common{vendor = 16#1AF4, device_id = 16#1041}}, _Args) -> true.

attach(Device, _Args) ->
	Common = Device#pci_device.common,
	Capabilities = parse_capabilities(Common#pci_common.capabilities, #{}),
	{ok, CommonMap} = map_structure(common_cfg, Device, Capabilities),
	io:format("got here ~w~n", [CommonMap]),
	crazierl:bcopy_to(CommonMap, 0, <<0:32/little>>),
	<<FeaturesLo:32/little>> = crazierl:bcopy_from(CommonMap, 4, 4),
	crazierl:bcopy_to(CommonMap, 0, <<1:32/little>>),
	<<FeaturesHi:32/little>> = crazierl:bcopy_from(CommonMap, 4, 4),
	Features = parse_features(<<FeaturesHi:32, FeaturesLo:32>>, []),
	io:format("~w~n", [Features]),
	io:format("~w~n", [Capabilities]),
	loop(Device).

parse_capabilities([{vendor_specific, <<RawType:8, Bar:8, _:3/binary, Offset:32/little, Length:32/little, _/binary>>}|T], Map) ->
	Type = virtio_cfg_type(RawType),
	case maps:is_key(Type, Map) of
		true -> parse_capabilities(T, Map);
		false -> parse_capabilities(T, Map#{Type => #virtio_pci_cap{bar = Bar, offset = Offset, length = Length}})
	end;
parse_capabilities([_H|T], Map) -> parse_capabilities(T, Map);
parse_capabilities([], Map) -> Map.

map_structure(Key, #pci_device{bars = Bars}, Caps) ->
	Cap = maps:get(Key, Caps),
	Bar = element(Cap#virtio_pci_cap.bar + 1, Bars),
	io:format("bar~B, ~w~n", [Cap#virtio_pci_cap.bar, Bar]),
	true = (Bar#pci_mem_bar.size >= (Cap#virtio_pci_cap.offset + Cap#virtio_pci_cap.length)),
	io:format("crazierl:map(~.16B, ~B)~n", [Bar#pci_mem_bar.base + Cap#virtio_pci_cap.offset, Cap#virtio_pci_cap.length]),
	crazierl:map(Bar#pci_mem_bar.base + Cap#virtio_pci_cap.offset, Cap#virtio_pci_cap.length).

loop(Device) ->
	timer:sleep(10000),
	loop(Device).


virtio_cfg_type(1) -> common_cfg;
virtio_cfg_type(2) -> notify_cfg;
virtio_cfg_type(3) -> isr_cfg;
virtio_cfg_type(4) -> device_cfg;
virtio_cfg_type(5) -> pci_cfg;
virtio_cfg_type(O) -> {reserved, O}.

parse_features(<<1:1, Rest/bitstring>>, Acc) ->
	parse_features(Rest, [feature_flag(bit_size(Rest)) | Acc]);
parse_features(<<0:1, Rest/bitstring>>, Acc) -> parse_features(Rest, Acc);
parse_features(<<>>, Acc) -> Acc.

feature_flag(0) -> checksum;
feature_flag(1) -> guest_checksum;
feature_flag(2) -> control_guest_offloads;
feature_flag(3) -> mtuy;
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
feature_flag(X) -> {unknown, X}.
