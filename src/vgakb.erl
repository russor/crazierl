-module (vgakb).

-export ([start/2, init/3]).
-record (s, {
		owner,
		io_port,
		irq,
		mods,
		current_index,
		cursor_updated,
		in_buffer, out_buffer, frame_buffer,
		last_line_compare = -1
}).

-record (mods, {
	left_ctrl = false,
	left_shift = false,
	right_shift = false,
	left_alt = false,
	caps_lock = false,
	num_lock = false,
	scroll_lock = false
}).

-define(VGA_FB, 16#a0000).
-define(VGA_LEN, 16#20000).
-define(VGA_ELEMENTS, (?VGA_LEN div 2)).
-define(VGA_ROWS, 25).
-define(VGA_COLS, 80).
-define(VGA_MEM_COLS, 128).
-define(TERM_COLOR, 16#F). % Black background, White foreground

start(IoPort, Interrupt) ->
	spawn(?MODULE, init, [self(), IoPort, Interrupt]).

init(Owner, IoPort, Interrupt) ->
	register(?MODULE, self()),
	{ok, InterruptSocket} = gen_udp:open(0, [
	        {inet_backend, inet},
		{ifaddr, {local, Interrupt}},
		{active, true}
	]),
	VgaInited = init_vga(),
	Key = crazierl:inb(IoPort), % read and process any pending keystroke
	loop(key_decode(VgaInited#s{owner = Owner, io_port = IoPort, irq = InterruptSocket, mods = #mods{}}, Key)).

init_vga() ->
	% assumption: kernel is already writing to VGA
	% vga memory map is set to 16#a0000 - 16#bffff
	% horizontal offset set to 128
	% vga is in text mode
	% cursor is placed by kernel where we should start writing new data
	% CRT controller register is at 16#3D4

	{ok, Map} = crazierl:map(?VGA_FB, ?VGA_LEN),

	% fetch cursor location
	crazierl:outb(16#3D4, 16#F),
	CurIndexLo = crazierl:inb(16#3D5),
	crazierl:outb(16#3D4, 16#E),
	CurIndexHi = crazierl:inb(16#3D5),
	<<CurIndex:16>> = <<CurIndexHi:8, CurIndexLo:8>>,

	#s{current_index = CurIndex, cursor_updated = false, in_buffer = <<>>, out_buffer = <<>>, frame_buffer = Map}.


loop(State = #s{irq = Irq, io_port = Port, in_buffer = IB}) ->
	Timeout = case {State#s.out_buffer, State#s.cursor_updated} of
		{<<>>, false} -> infinity;
		_ -> 0
	end,
	NewState = receive
		{udp, Irq, _, _, _} ->
			Key = crazierl:inb(Port),
			key_decode(State, Key);
		Data when is_binary(Data) ->
			output_decode(State#s{in_buffer = <<IB/binary, Data/binary>>})
	after Timeout ->
		flush(State)
	end,
	loop(NewState).

output_decode(State = #s{out_buffer = OB, current_index = CI})
	when (CI band (?VGA_MEM_COLS - 1)) + (size(OB) div 2) >= ?VGA_COLS ->
	output_decode(flush(State));

output_decode(State = #s{in_buffer = <<CtrlChar:8, _/binary>>, out_buffer = OB})
	when CtrlChar =< 31, OB /= <<>> ->
	output_decode(flush(State));

output_decode(State = #s{in_buffer = <<$\r, IB/binary>>, current_index = CI}) ->
	output_decode(State#s{in_buffer = IB, cursor_updated = true, current_index = CI band (bnot(?VGA_MEM_COLS - 1))});

output_decode(State = #s{in_buffer = <<$\b, IB/binary>>, current_index = CI}) ->
	output_decode(State#s{in_buffer = IB, cursor_updated = true, current_index = CI - 1});

output_decode(State = #s{in_buffer = <<$\n, IB/binary>>}) ->
	output_decode(newline(State#s{in_buffer = IB, cursor_updated = true}));

% ignore bell
output_decode(State = #s{in_buffer = <<$\^g, IB/binary>>}) ->
	output_decode(State#s{in_buffer = IB});

output_decode(State = #s{in_buffer = <<$\e, $[, $C, IB/binary>>, current_index = CI}) ->
	output_decode(State#s{in_buffer = IB, cursor_updated = true, current_index = CI + 1});

output_decode(State = #s{in_buffer = <<$\e, $[, $A, IB/binary>>, current_index = CI}) ->
	NewCI = (CI - ?VGA_MEM_COLS) band (?VGA_ELEMENTS - 1),
	output_decode(State#s{in_buffer = IB, cursor_updated = true, current_index = NewCI});

% ignore erase until end of screen?
output_decode(State = #s{in_buffer = <<$\e, $[, $J, IB/binary>>}) ->
	output_decode(State#s{in_buffer = IB});

output_decode(State = #s{in_buffer = <<$\e, A, B, C, IB/binary>>}) ->
%	io:format("vgakb ignoring ESC + 16#~2.16.0B~2.16.0B~2.16.0B (~s)~n", [A, B, C, [A,B,C]]),
	output_decode(State#s{in_buffer = IB});

output_decode(State = #s{in_buffer = <<$\e, _/binary>>}) ->
	State;

output_decode(State = #s{in_buffer = <<Char:8, IB/binary>>, out_buffer = OB}) ->
	output_decode(State#s{in_buffer = IB, out_buffer = <<OB/binary, Char:8, ?TERM_COLOR:8>>});

output_decode(State = #s{in_buffer = <<>>}) ->
	State.


flush (State = #s{out_buffer = <<>>, cursor_updated = true, current_index = CI}) ->  % all data output, need to move cursor
	% set cursor location
	CursorIndex = CI,
	<<CurIndexHi:8, CurIndexLo:8>> = <<CursorIndex:16>>,
	crazierl:outb(16#3D4, 16#E),
	crazierl:outb(16#3D5, CurIndexHi),
	crazierl:outb(16#3D4, 16#F),
	crazierl:outb(16#3D5, CurIndexLo),
	State#s{cursor_updated = false};

flush (State = #s{out_buffer = OB, frame_buffer = FB}) ->
	crazierl:bcopy_to(FB, (State#s.current_index * 2), OB),
	AddedIndex = State#s.current_index + (size(OB) div 2),
	if
		AddedIndex band (?VGA_MEM_COLS - 1) >= ?VGA_COLS ->
			newline(State#s{out_buffer = <<>>, cursor_updated = true, current_index = AddedIndex});
		true ->
			State#s{out_buffer = <<>>, cursor_updated = true, current_index = AddedIndex}
	end.

newline(State = #s{current_index = OldIndex, frame_buffer = FB}) ->
	Index = case (OldIndex band (bnot (?VGA_MEM_COLS - 1))) + ?VGA_MEM_COLS of
		?VGA_ELEMENTS -> 0;
		Other -> Other
	end,

	% clear next line
	crazierl:bcopy_to(FB, (Index * 2), binary:copy(<<" ", ?TERM_COLOR:8>>, ?VGA_COLS)),
	RawDisplayIndex = Index - (?VGA_MEM_COLS * (?VGA_ROWS - 1)),
	{LineCompare, DisplayIndex} = if
		RawDisplayIndex < 0 ->
			{16 * (-RawDisplayIndex div ?VGA_MEM_COLS), RawDisplayIndex + ?VGA_ELEMENTS};
		true ->
			{16#3FF, RawDisplayIndex}
	end,
	case State#s.last_line_compare of
		LineCompare -> ok;
		_ ->
			crazierl:outb(16#3D4, 16#7),
			Overflow = crazierl:inb(16#3D5),
			NewOverflow = case ((LineCompare band 16#100) /= 0) of
				true -> Overflow bor  2#00010000;
				false ->Overflow band 2#11101111
			end,
			Overflow /= NewOverflow andalso crazierl:outb(16#3D5, NewOverflow),

			crazierl:outb(16#3D4, 16#9),
			MaxScan = crazierl:inb(16#3D5),
			NewMaxScan = case ((LineCompare band 16#200) /= 0) of
				true -> MaxScan bor  2#01000000;
				false ->MaxScan band 2#10111111
			end,
			MaxScan /= NewMaxScan andalso crazierl:outb(16#3D5, NewMaxScan),

			crazierl:outb(16#3D4, 16#18),
			crazierl:outb(16#3D5, LineCompare band 16#FF)
	end,

	<<IndexHi:8, IndexLo:8>> = <<DisplayIndex:16>>,
	crazierl:outb(16#3D4, 16#C),
	crazierl:outb(16#3D5, IndexHi),
	crazierl:outb(16#3D4, 16#D),
	crazierl:outb(16#3D5, IndexLo),
	State#s{out_buffer = <<>>, cursor_updated = true, last_line_compare = LineCompare, current_index = Index}.


key_decode(State, 16#e0) -> State; %ignore escapes for the moment
key_decode(State, 16#e1) -> State;

key_decode(State = #s{mods = Mods}, RawKey) ->
	{KeyMake, Key} = if
		RawKey band 16#80 /= 0 -> {false, RawKey band 16#7F };
		true -> {true, RawKey}
	end,

	% scancode reference https://www.win.tue.nl/~aeb/linux/kbd/scancodes-1.html
	ScanTable = if
		Mods#mods.left_ctrl -> {
			$\e,
			{}, 0, {}, {}, {}, $\^^, {}, {}, {}, {}, $\^_, {}, $\b,
			{}, $\^Q, $\^W, $\^E, $\^R, $\^T, $\^Y, $\^U, $\^I, $\^O, $\^P, $\^[, $\^], $\r,
			left_ctrl, $\^A, $\^S, $\^D, $\^F, $\^G, $\^H, $\^J, $\^K, $\^L, {}, {}, {},
			left_shift, $\^\, $\^Z, $\^X, $\^C, $\^V, $\^B, $\^N, $\^M, {}, {}, $\^?, right_shift,
			{}, left_alt, $\s, caps_lock,
			{f, 1}, {f, 2}, {f, 3}, {f, 4}, {f, 5},
			{f, 6}, {f, 7}, {f, 8}, {f, 9}, {f, 10},
			num_lock, scroll_lock,
			{f, home}, {f, up}, {f, pgup}, {},
			{f, left}, {f, keypad_5}, {f, right}, {},
			{f, 'end'}, {f, down}, {f, pgdn},
			{f, ins}, {f, del},
			0, 0, 0, % Alt-SysRq, ??, ??
			{f, 11}, {f, 12}
		};
		Mods#mods.left_shift orelse Mods#mods.right_shift -> {
			$\e,
			$!, $@, $#, $$, $%, $^, $&, $*, $(, $), $_, $+, $\b,
			$\t, $Q, $W, $E, $R, $T, $Y, $U, $I, $O, $P, ${, $}, $\r,
			left_ctrl, $A, $S, $D, $F, $G, $H, $J, $K, $L, $:, $\", $~,
			left_shift, $|, $Z, $X, $C, $V, $B, $N, $M, $<, $>, $?, right_shift,
			$*, left_alt, $\s, caps_lock,
			{f, 1}, {f, 2}, {f, 3}, {f, 4}, {f, 5},
			{f, 6}, {f, 7}, {f, 8}, {f, 9}, {f, 10},
			num_lock, scroll_lock,
			{f, home}, {f, up}, {f, pgup}, $-,
			{f, left}, {f, keypad_5}, {f, right}, $+,
			{f, 'end'}, {f, down}, {f, pgdn},
			{f, ins}, {f, del},
			0, 0, 0, % Alt-SysRq, ??, ??
			{f, 11}, {f, 12}
		};
		true -> {
			$\e,
			$1, $2, $3, $4, $5, $6, $7, $8, $9, $0, $-, $=, $\b,
			$\t, $q, $w, $e, $r, $t, $y, $u, $i, $o, $p, $[, $], $\r,
			left_ctrl, $a, $s, $d, $f, $g, $h, $j, $k, $l, $;, $\', $`,
			left_shift, $\\, $z, $x, $c, $v, $b, $n, $m, $,, $., $/, right_shift,
			$*, left_alt, $\s, caps_lock,
			{f, 1}, {f, 2}, {f, 3}, {f, 4}, {f, 5},
			{f, 6}, {f, 7}, {f, 8}, {f, 9}, {f, 10},
			num_lock, scroll_lock,
			{f, home}, {f, up}, {f, pgup}, $-,
			{f, left}, {f, keypad_5}, {f, right}, $+,
			{f, 'end'}, {f, down}, {f, pgdn},
			{f, ins}, {f, del},
			0, 0, 0, % Alt-SysRq, ??, ??
			{f, 11}, {f, 12}
		}
	end,

	if
		Key > 0 andalso Key =< tuple_size(ScanTable) ->
			DecodedKey = element(Key, ScanTable),
			key_process(State, KeyMake, DecodedKey);
		RawKey == 16#FA ->
			State;
		true ->
			io:format("couldn't handle scancode ~.16B (~.16B)~n", [Key, RawKey]),
			State
	end.

key_process(State = #s {mods = Mods}, Bool, Modifier) when is_atom(Modifier) ->
	NewMods = case Modifier of
		left_ctrl -> Mods#mods{left_ctrl = Bool};
		left_shift -> Mods#mods{left_shift = Bool};
		right_shift -> Mods#mods{right_shift = Bool};
		left_alt -> Mods#mods{left_alt = Bool};
		caps_lock when Bool == true -> Mods#mods{caps_lock = bnot Mods#mods.caps_lock};
		num_lock when Bool == true -> Mods#mods{num_lock = bnot Mods#mods.num_lock};
		scroll_lock when Bool == true -> Mods#mods{scroll_lock = bnot Mods#mods.scroll_lock};
		_ -> Mods
	end,
	State#s{ mods = NewMods };
key_process(State, false, _DecodedKey) -> State; % ignore other key up events
key_process(State, true, DecodedKey) when is_integer(DecodedKey) ->
	State#s.owner ! { self(), [DecodedKey] },
	State;
key_process(State, true, {f, up}) ->
	State#s.owner ! { self(), ["\e[A"]},
	State;
key_process(State, true, {f, down}) ->
	State#s.owner ! { self(), ["\e[B"]},
	State;
key_process(State, true, {f, right}) ->
	State#s.owner ! { self(), ["\e[C"]},
	State;
key_process(State, true, {f, left}) ->
	State#s.owner ! { self(), ["\e[D"]},
	State;
key_process(State, true, DecodedKey) ->
	io:format("key down ~2000p~n", [DecodedKey]),
	State.
