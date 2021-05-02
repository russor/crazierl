-module (rtl_8168).

-include ("pci.hrl").
-export([check/2, attach/2]).
-record (q, {map, idx, avail}).

% TODO split virtio and net bits

-define (TX_DESC, 16#20).
-define (COMMAND, 16#37).
-define (TX_POLL, 16#38).
-define (IRQ_MASK, 16#3C).
-define (IRQ_STATUS, 16#3E).
-define (TX_CFG, 16#40).
-define (RX_CFG, 16#44).
-define (CFG_ENABLE, 16#50).
-define (RX_MAX, 16#DA).
-define (RX_DESC, 16#E4).
-define (TX_MAX, 16#EC).

-define (Q_LEN, 32).
-define (Q_DESC_SIZE, 16).
-define (Q_DESC_TABLE_LEN, (?Q_DESC_SIZE * ?Q_LEN)).
-define (Q_MAX_PACKET, 1514). % 6 byte dest mac, 6 byte source mac, 2 byte protocol, 1500 payload
-define (Q_BUFFER_SIZE, (?Q_MAX_PACKET + ?ALIGN(?Q_MAX_PACKET, 8))). % max packet + 12 byte header
-define (Q_BUFFER_LEN, (?Q_BUFFER_SIZE * ?Q_LEN)).

-define (Q_TOTAL_LEN, (?Q_DESC_TABLE_LEN + ?Q_BUFFER_LEN)).


-define(ALIGN(Value, Align), 
	(case Value rem Align of
		0 -> Value;
		_ -> Value + Align - (Value rem Align)
	end)).

check(#pci_device{common = #pci_common{vendor = 16#10EC, device_id = 16#8168}}, _Args) -> true.

attach(Device, _Args) ->
	register(?MODULE, self()),
	Common = Device#pci_device.common,

	Bar = element(3, Device#pci_device.bars),
	{ok, MMIO} = crazierl:map(Bar#pci_mem_bar.base, Bar#pci_mem_bar.size),

	reset(MMIO),

	MacAddr = crazierl:bcopy_from(MMIO, 0, 6),

	{Socket, Int} = crazierl:open_interrupt(0),
	pci:enable_msix(Common, 0, Int),
	
	RxQ = setup_q(read),
	TxQ = setup_q(write),
	
	% init values taken from https://wiki.osdev.org/RTL8169
        crazierl:bcopy_to(MMIO, ?IRQ_MASK, <<16#9F:16/little>>), % enable TX/RX descriptor unavailable, error and ok
	
	crazierl:bcopy_to(MMIO, ?CFG_ENABLE, <<16#c0>>), % unlock configuration

	crazierl:bcopy_to(MMIO, ?RX_CFG, <<16#E70E:16/little>>), % unlimited dma, not promiscuous
	crazierl:bcopy_to(MMIO, ?RX_MAX, <<16#1FFF:16/little>>),
	{_, RxPhysical} = crazierl:map_addr(RxQ#q.map),
	crazierl:bcopy_to(MMIO, ?RX_DESC, <<RxPhysical:64/little>>),

	crazierl:bcopy_to(MMIO, ?COMMAND, <<16#C>>), % enable tx and rx
	crazierl:bcopy_to(MMIO, ?TX_CFG, <<16#0300700:32/little>>), % normal interframe gap, unlimited dma
	crazierl:bcopy_to(MMIO, ?TX_MAX, <<16#3B>>),
	{_, TxPhysical} = crazierl:map_addr(TxQ#q.map),
	crazierl:bcopy_to(MMIO, ?TX_DESC, <<TxPhysical:64/little>>),

	crazierl:bcopy_to(MMIO, ?CFG_ENABLE, <<16#0>>), % lock configuration

	application:set_env([
		{etcpip, [
			{ip, {0,0,0,0}},
			{netmask, {255, 255, 255, 255}},
			{gateway, {0, 0, 0, 0}},
			{mac,     MacAddr},
			{iface,   "re0"},
			{ip6, [
				{addr, "fe80::216:3eff:fe00:1234"}
			]}
		]}
	], [{persistent, true}]),
	etcpip_socket:start(),
	loop(Device, MMIO, MacAddr, Socket, RxQ, TxQ).


reset(MMIO) ->
	<<High:3, _:1, Lo:4>> = crazierl:bcopy_from(MMIO, ?COMMAND, 1),
	crazierl:bcopy_to(MMIO, ?COMMAND, <<High:3, 0:1, Lo:4>>),
	reset_loop(MMIO).

reset_loop(MMIO) ->
	<<Status:8>> = crazierl:bcopy_from(MMIO, ?COMMAND, 1),
	case Status band 16 of
		0 -> ok;
		16 -> reset_loop(MMIO)
	end.

%% tx queue full, don't process send messages
loop(Device, MMIO, MacAddr, Socket, RxQ, TxQ = #q{avail = 0}) ->
	{RxQ1, TxQ1} = receive
		{udp, Socket, _, _, _} ->
			%Status = crazierl:bcopy_from(MMIO, ?IRQ_STATUS, 2),
			crazierl:bcopy_to(MMIO, ?IRQ_STATUS, <<16#FFFF:16>>),
			{check_queue(RxQ, read), check_queue(TxQ, write)};
		{ping, From} ->
			From ! pong,
			{RxQ, TxQ}
	end,
	loop(Device, MMIO, MacAddr, Socket, RxQ1, TxQ1);

loop(Device, MMIO, MacAddr, Socket, RxQ, TxQ) ->
	{RxQ1, TxQ1} = receive
		{udp, Socket, _, _, _} ->
			%Status = crazierl:bcopy_from(MMIO, ?IRQ_STATUS, 2),
			%case Status of 
			%	<<1, 0>> -> ok;
			%	_ -> io:format("got irq ~p~n", [Status])
			%end,
			crazierl:bcopy_to(MMIO, ?IRQ_STATUS, <<16#FFFF:16>>),
			{check_queue(RxQ, read), check_queue(TxQ, write)};
		{'$gen_call', From, {send, {DestMac, EtherType}, Payload}} ->
			Packet = <<DestMac:48, MacAddr:48, EtherType:16, Payload/binary>>,
			gen:reply(From, ok),
			{RxQ, add_to_queue(MMIO, TxQ, Packet)};
		{'$gen_call', From, {send, Data}} ->
			Packet = iolist_to_binary(Data),
			gen:reply(From, ok),
			{RxQ, add_to_queue(MMIO, TxQ, Packet)};
		{ping, From} ->
			From ! pong,
			{RxQ, TxQ};
		Other ->
			io:format("got message ~p", [Other]),
			{RxQ, TxQ}
	end,
	loop(Device, MMIO, MacAddr, Socket, RxQ1, TxQ1).


setup_q(Type) ->
	{ok, Map} = crazierl:map(0, ?Q_TOTAL_LEN),
	{_Virtual, Physical} = crazierl:map_addr(Map),
	{Avail, Own} = case Type of
		read -> {0, 1};
		write -> {?Q_LEN, 0}
	end,
	DescriptorTable = virtq_descriptors(Physical + ?Q_DESC_TABLE_LEN, Own, 0, <<>>),
	crazierl:bcopy_to(Map, 0, <<DescriptorTable/binary>>),
	#q{map = Map, idx = 0, avail = Avail}.

virtq_descriptors(_, _, ?Q_LEN, Acc) -> Acc;
virtq_descriptors(Physical, Own, Index, Acc) ->
        End = case Index + 1 of
        	?Q_LEN -> 1;
        	_ -> 0
        end,

	Acc0 = <<Acc/binary, ?Q_BUFFER_SIZE:16/little, 0:8, Own:1, End:1, 0:6, 0:32, Physical:64/little>>,
	virtq_descriptors(Physical + ?Q_BUFFER_SIZE, Own, Index + 1, Acc0).

add_to_queue(MMIO, Queue = #q{map = Map, idx = Idx, avail = Avail}, Packet) when size(Packet) =< ?Q_MAX_PACKET ->
	crazierl:bcopy_to(Map, ?Q_DESC_TABLE_LEN + (?Q_BUFFER_SIZE * Idx), Packet),
	End = case Idx + 1 of
		?Q_LEN -> 1;
		_ -> 0
	end,
	
	crazierl:bcopy_to(Map, ?Q_DESC_SIZE * Idx, <<(size(Packet)):16/little, 0:8, 1:1, End:1, 1:1, 1:1, 0:4>>),
	
	%io:format("wrote packet~n", []),
	crazierl:bcopy_to(MMIO, ?TX_POLL, <<16#40>>),

	Queue#q{idx = (Idx + 1) rem ?Q_LEN, avail = Avail - 1};
add_to_queue(_MMIO, Queue, Packet) ->
	io:format("packet too big (~B > ~B), dropping ~w~n", [size(Packet), ?Q_MAX_PACKET, Packet]),
	Queue.

check_queue(Queue = #q{avail = ?Q_LEN}, write) -> Queue;
check_queue(Queue = #q{map = Map, avail = Avail, idx = Idx}, write) ->
	Header = crazierl:bcopy_from(Map, ?Q_DESC_SIZE * ((Idx + Avail) rem ?Q_LEN), 4),
	case Header of
		<<_:24, 0:1, _:7>> -> 
			Queue#q{avail = Avail + 1};
		_ -> Queue
	end;

check_queue(Queue = #q{map = Map, idx = Idx}, read) ->
	Header = crazierl:bcopy_from(Map, ?Q_DESC_SIZE * Idx, 4),
	%io:format("header ~w, Type ~s, idx ~B~n", [Header, Type, Idx]),
	case Header of
		<<_:24, 0:1, _:7>> -> process_packet (Queue, Header);
		_ -> Queue
	end.

process_packet(Queue = #q{map = Map, idx = Idx}, Header) ->
	%io:format("reading used ring item (read) ~B ~w~n", [Idx, Header]),
	
	<<LengthAndFlags:16/little,
	  _:1, _RXWatchDogExp:1, _RXErr:1, _Runt:1, _CrcError:1, _Protocol:2, _IPChecksum:1,
	  0:1, End:1, FirstSegment:1, LastSegment:1, _Multicast:1, _ToMe:1, _Broacast:1, _:1>> = Header,
        
        Length = LengthAndFlags band 16#3FF,
        Packet = crazierl:bcopy_from(Map, ?Q_DESC_TABLE_LEN + (?Q_BUFFER_SIZE * Idx), Length),
	case {FirstSegment, LastSegment} of
		{1, 1} -> ok;
		_ -> io:format("funny business ~w~n", [Header])
	end,
	
	Empty = <<?Q_BUFFER_SIZE:16/little, 0:8, 1:1, End:1, 0:6>>,
	crazierl:bcopy_to(Map, ?Q_DESC_SIZE * Idx, Empty),

        eth:recv(Packet),
	NewIdx = (Idx + 1) rem ?Q_LEN,
	check_queue(Queue#q{idx = NewIdx}, read).
