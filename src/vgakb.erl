-module (vgakb).

-export ([start/2, init/3]).
-record (s, {
		owner,
		io_port,
		irq,
		mods
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

start(IoPort, Interrupt) ->
	spawn(?MODULE, init, [self(), IoPort, Interrupt]).

init(Owner, IoPort, Interrupt) ->
	{ok, InterruptSocket} = gen_udp:open(0, [
		{ifaddr, {local, Interrupt}},
		{active, true}
	]),
	Key = crazierl:inb(IoPort), % read and process any pending keystroke
	loop(key_decode(#s{owner = Owner, io_port = IoPort, irq = InterruptSocket, mods = #mods{}}, Key)).

loop(State = #s{irq = Irq, io_port = Port}) ->
	NewState = receive
		{udp, Irq, _, _, _} ->
			Key = crazierl:inb(Port),
			key_decode(State, Key);
		{stdin, _Data} ->
			State;
		{stdout, _Data} ->
			State;
		{stderr, _Data} ->
			State
	end,
	loop(NewState).

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
key_process(State, true, DecodedKey) ->
	io:format("key down ~2000p~n", [DecodedKey]),
	State.
