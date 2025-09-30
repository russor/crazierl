-module (comport).

-export ([start/2, init/3]).
-record (s, {
		owner,
		io_port,
		irq,
		buffer
}).

start(IoPort, Interrupt) ->
	spawn(?MODULE, init, [self(), IoPort, Interrupt]).

init(Owner, IoPort, Interrupt) ->
	{ok, InterruptSocket} = gen_udp:open(0, [
	        {inet_backend, inet},
		{ifaddr, {local, Interrupt}},
		{active, true}
	]),

	% init copied from kernel.c
	crazierl:outb(IoPort + 1, 16#00), % Disable all interrupts
        crazierl:outb(IoPort + 3, 16#80), % Enable DLAB (set baud rate divisor)
        crazierl:outb(IoPort + 0, 16#0C), % Set divisor to 16#0C (lo byte) 9600 baud
        crazierl:outb(IoPort + 1, 16#00), %                16#00 (hi byte)

        crazierl:outb(IoPort + 3, 16#03), % 8 bits, no parity, one stop bit
        crazierl:outb(IoPort + 2, 16#C7), % Enable FIFO, clear them, with 14-byte threshold
        crazierl:outb(IoPort + 4, 16#1B), % Loopback enabled, IRQs enabled, RTS/DSR set

        crazierl:outb(IoPort, 16#AE), % Write to loopback
        16#AE = crazierl:inb(IoPort), % Read from loopback

        crazierl:outb(IoPort + 4, 16#0B), % Loopback disabled, IRQs enabled, RTS/DSR set

	crazierl:outb(IoPort + 1, 2#11), % request irq for data avail and transmitter empty
	crazierl:outb(IoPort, 16#13), % XOFF
	loop(port_loop(#s{owner = Owner, io_port = IoPort, irq = InterruptSocket, buffer = <<>>})).

loop(State = #s{buffer = B, irq = Irq}) ->
	NewState = receive
		{udp, Irq, _, _, _} ->
			port_loop(State);
		Data when is_binary(Data) ->
			port_loop(State#s{buffer = <<B/binary, Data/binary>>})
	end,
	loop(NewState).

port_loop(State = #s{io_port = Port, buffer = B}) ->
	Status = crazierl:inb(Port + 5),
	case {Status band 1, Status band 32} of
		{1, _} ->
			case Status band 2 of
				2 -> error_logger:error_msg("com port ~.16B overrun Status: ~.16B", [Port, Status]);
				0 -> ok
			end,
			State#s.owner ! {self(), [crazierl:inb(Port)]},
			port_loop(State); % prioritize reading over writing!
		{0, 32} when B /= <<>> ->
			<<C:8, NewB/binary>> = B,
			crazierl:outb(Port, C),
			port_loop(State#s{buffer = NewB});
		{0, 32} ->
			% clear possible transmit OK interrupt
			_InterruptId = crazierl:inb(Port + 2),
			State;
		_ -> State
	end.
