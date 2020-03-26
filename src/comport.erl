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
		{ifaddr, {local, Interrupt}},
		{active, true}
	]),
	crazierl:outb(IoPort + 1, 2#11), % request irq for data avail and transmitter empty
	loop(port_loop(#s{owner = Owner, io_port = IoPort, irq = InterruptSocket, buffer = <<>>})).

loop(State = #s{buffer = B, irq = Irq}) ->
	NewState = receive
		{udp, Irq, _, _, _} ->
			port_loop(State);
		{stdin, Data} ->
			port_loop(State#s{buffer = <<B/binary, Data/binary>>});
		{stdout, Data} ->
			port_loop(State#s{buffer = <<B/binary, Data/binary>>});
		{stderr, Data} ->
			port_loop(State#s{buffer = <<B/binary, Data/binary>>})
	end,
	loop(NewState).

port_loop(State = #s{io_port = Port, buffer = B}) ->
	Status = crazierl:inb(Port + 5),
	case Status band 1 of
		1 ->
			State#s.owner ! {self(), [crazierl:inb(Port)]},
			port_loop(State); % prioritize reading over writing!
		0 -> ok
	end,
	case Status band 32 of
		32 when B /= <<>> ->
			<<C:8, NewB/binary>> = B,
			crazierl:outb(Port, C),
			port_loop(State#s{buffer = NewB});
		32 ->
			% clear possible transmit OK interrupt
			_InterruptId = crazierl:inb(State#s.io_port + 2),
			State;
		_ -> State
	end.
