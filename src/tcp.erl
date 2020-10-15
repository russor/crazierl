-module(tcp).

-export([start/0]).
-behavior(gen_server).

-export ([init/1, handle_cast/2, handle_call/3]).

-export ([listen/2]).
-export ([process/4]).
-export ([format_packet/4]).
-export ([controlling_process/2, send/2, close/1]).

% int32/u32 borrowed from dist_util.erl/inet_int.hrl
-define(int32(X),
	{((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
	 ((X) bsr 8) band 16#ff, (X) band 16#ff}).
-define(u32(X3,X2,X1,X0),
	(((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

-define (TCP_PROTO, 6).
-define (FIN, 1).
-define (SYN, 2).
-define (RST, 4).
-define (PSH, 8).
-define (ACK, 16).
-define (URG, 32).
-define (SEQACK_MASK, ((1 bsl 32) - 1)).
-define (MAX_ACK, (1 bsl 31)).

-record (established, {pid, snd_una, rcv_next, buf}).
-record (fin_wait1, {pid, snd_una, rcv_next, buf}).
-record (fin_wait2, {pid, snd_una, rcv_next}).

start() ->
	case gen_server:start({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> Pid;
		{error, {already_started, Pid}} -> Pid;
		Other -> Other
	end.

init ([]) ->
	process_flag(trap_exit, true),
	?MODULE = ets:new(?MODULE, [set, protected, named_table]),
	{ok, #{} }.

listen(Port, Pid) when is_pid(Pid), is_integer(Port), Port > 0, Port < 65535 ->
	gen_server:call(?MODULE, {listen, Port, Pid}).

process(Source, Dest, TTL, <<SourcePort:16, DestPort:16, Seq:32, Ack:32,
			     Data:4, 0:3, Flags:9,
			     %NS:1, CWR:1, ECE:1, URG:1, ACK:1, PSH:1, RST:1, SYN:1, FIN:1,
			     Window:16, CheckSum:16, UrgPtr:16,
			     Options:(Data * 4 - 20)/binary, Payload/binary>> = Packet) ->

	format_packet(Source, Dest, TTL, Packet),
	PacketSize = size(Packet),

	case ip:ip_checksum(<<Source:32, Dest:32, 6:16, PacketSize:16, Packet/binary>>, 0) of
		0 -> gen_server:cast(?MODULE, {in, {Source, Dest, SourcePort, DestPort, Seq, Ack, Flags, Window, UrgPtr, Options, Payload}});
		_ -> ok
	end.

format_packet(Source, Dest, TTL, <<SourcePort:16, DestPort:16, Seq:32, Ack:32,
			     Data:4, 0:3, Flags:9,
			     %NS:1, CWR:1, ECE:1, URG:1, ACK:1, PSH:1, RST:1, SYN:1, FIN:1,
			     Window:16, CheckSum:16, UrgPtr:16,
			     Options:(Data * 4 - 20)/binary, Payload/binary>> = Packet) ->

	PacketSize = size(Packet),

	ChecksumStatus = case ip:ip_checksum(<<Source:32, Dest:32, 6:16, PacketSize:16, Packet/binary>>, 0) of
		0 -> ok;
		Other -> Other
	end,
	FlagField = format_flags(<<Flags:9>>, <<"NWEU.PRSF">>, []),
	io:format("tcp ~w:~B -> ~w:~B: Flags [~s], seq ~B, ack ~B, win ~B, urg ~B, ttl ~B, "
	          "checksum ~.16B (~p), options (~w) ~w, length ~B~n",
		[?int32(Source), SourcePort, ?int32(Dest), DestPort, FlagField, Seq, Ack, Window, UrgPtr,
		TTL, CheckSum, ChecksumStatus, Options, Data * 4 - 20, size(Payload)]).

format_flags(<<1:1, Flags/bitstring>>, <<C, Chars/binary>>, Acc) ->
	format_flags(Flags, Chars, [C|Acc]);
format_flags(<<0:1, Flags/bitstring>>, <<_, Chars/binary>>, Acc) ->
	format_flags(Flags, Chars, Acc);
format_flags(<<>>, <<>>, []) -> "none";
format_flags(<<>>, <<>>, Acc) -> Acc.

handle_call({listen, Port, Pid}, _From, State) ->
	Reply = ets:insert_new(?MODULE, {Port, listen, Pid}),
	{reply, Reply, State};
handle_call({controlling_process, Socket, NewPid}, _From, State) ->
	Reply = case ets:lookup(?MODULE, Socket) of
		[{_, #established{} = TcpState}] ->
			ets:insert(?MODULE, {Socket, TcpState#established{pid = NewPid}});
		_ ->
			{error, badarg}
	end,
	{reply, Reply, State}.

% check ack only first
handle_cast({in, {Source, Dest, SourcePort, DestPort, Seq, Ack, Flags, Window, UrgPtr, Options, Payload}}, State)
	when Flags == ?ACK; Flags == ?ACK bor ?PSH ->
	Key = {Source, Dest, SourcePort, DestPort},
	case ets:lookup(?MODULE, Key) of
		[{_, #established{pid = Pid, rcv_next = Seq, snd_una = Una, buf = Buf} = TcpState}] -> % fast path established, ACK only, right sequence
			Next = case Payload of
				<<>> -> Seq;
				_ ->
					Pid ! {tcp, Key, Payload},
					N = (Seq + size(Payload)) band ?SEQACK_MASK,
					reply(Key, Una + size(Buf), N, ?ACK, 65535, <<>>, <<>>),
					N
			end,
			ets:insert(?MODULE, {Key, process_ack(Ack, TcpState#established{rcv_next = Next})});
		[{_, #fin_wait1{pid = Pid, rcv_next = Seq, snd_una = Una, buf = Buf} = TcpState}] ->
			Next = case Payload of
				<<>> -> Seq;
				_ ->
					Pid ! {tcp, Key, Payload},
					N = (Seq + size(Payload)) band ?SEQACK_MASK,
					reply(Key, Una + size(Buf) + 1, N, ?ACK, 65535, <<>>, <<>>),
					N
			end,
			ets:insert(?MODULE, {Key, process_ack(Ack, TcpState#fin_wait1{rcv_next = Next})});
		[{_, syn_received, Seq, Ack}] ->
			case ets:lookup(?MODULE, DestPort) of
				[{_, listen, Pid}] ->
					ets:insert(?MODULE, {Key, #established{pid = Pid, rcv_next = Seq + size(Payload), snd_una = Ack, buf = <<>>}}),
					Pid ! {tcp_open, Key, Payload},
					reply(Key, Ack, Seq, ?ACK, 65535, <<>>, <<>>);
				_ ->
					ets:delete(Key),
					reply(Key, Ack, Seq, ?RST, 0, <<>>, <<>>)
			end;
		[] -> io:format("should send reset for ACK~n")
	end,
	{noreply, State};

% SYN
handle_cast({in, {Source, Dest, SourcePort, DestPort, Seq, 0 = Ack, ?SYN, Window, UrgPtr, Options, Payload}}, State) ->
	Key = {Source, Dest, SourcePort, DestPort},
	case ets:lookup(?MODULE, Key) of
		[{_, syn_received, S, MyISS}] when S == Seq + 1 ->
			reply(Key, MyISS - 1, Seq + 1, ?SYN bor ?ACK, 65535, <<>>, <<>>);
		[Other] ->
			io:format("ignoring syn on connection ~w~n", [Other]);
		[] ->
			case ets:lookup(?MODULE, DestPort) of
				[{_, listen, Pid}] ->
					MyISS = rand:uniform(1000), % FIXME should be crypto random
					ets:insert(?MODULE, {Key, syn_received, Seq +1, MyISS}),
					reply(Key, MyISS - 1, Seq + 1, ?SYN bor ?ACK, 65535, <<>>, <<>>);
				[] ->
					reply(Key, 0, Seq + 1, ?RST, 0, <<>>, <<>>)
			end
	end,
	{noreply, State};

handle_cast({in, {Source, Dest, SourcePort, DestPort, Seq, Ack, ?RST bor ?ACK, Window, UrgPtr, Options, Payload}}, State) ->
	Key = {Source, Dest, SourcePort, DestPort},
	case ets:lookup(?MODULE, Key) of
		[{_, #established{pid = Pid, rcv_next = Seq} = TcpState}] ->
			Pid ! {tcp_close, Key},
			ets:delete(?MODULE, Key);
		[{_, #fin_wait1{pid = Pid, rcv_next = Seq} = TcpState}] ->
			Pid ! {tcp_close, Key},
			ets:delete(?MODULE, Key);
		[{_, #fin_wait2{pid = Pid, rcv_next = Seq} = TcpState}] ->
			Pid ! {tcp_close, Key},
			ets:delete(?MODULE, Key)
	end,
	{noreply, State};

handle_cast({in, {Source, Dest, SourcePort, DestPort, Seq, Ack, ?FIN bor ?ACK, Window, UrgPtr, Options, Payload}}, State) ->
	Key = {Source, Dest, SourcePort, DestPort},
	case ets:lookup(?MODULE, Key) of
		[{_, #established{pid = Pid, snd_una = Una, rcv_next = Seq} = TcpState}] ->
			reply(Key, Una, Seq + 1, ?ACK, 0, <<>>, <<>>),
			Pid ! {tcp_close, Key},
			ets:delete(?MODULE, Key); % should time_wait
		[{_, #fin_wait1{pid = Pid, snd_una = Una, rcv_next = Seq} = TcpState}] ->
			reply(Key, Una, Seq + 1, ?ACK, 0, <<>>, <<>>),
			Pid ! {tcp_close, Key},
			ets:delete(?MODULE, Key); % should time_wait
		[{_, #fin_wait2{pid = Pid, snd_una = Una, rcv_next = Seq} = TcpState}] ->
			reply(Key, Una, Seq + 1, ?ACK, 0, <<>>, <<>>),
			Pid ! {tcp_close, Key},
			ets:delete(?MODULE, Key); % should time_wait
		[] ->
			reply(Key, 0, Seq + 1, ?RST bor ?ACK, 0, <<>>, <<>>)
	end,
	{noreply, State};

handle_cast({out, Socket, Payload}, State) ->
	case ets:lookup(?MODULE, Socket) of
		[{_, #established {rcv_next = Ack, snd_una = Una, buf = Buf} = TcpState}] ->
			reply(Socket, Una + size(Buf), Ack, ?ACK, 65535, <<>>, Payload),
			ets:insert(?MODULE, {Socket, TcpState#established{buf = <<Buf/binary, Payload/binary>>}});
		_ -> ok
	end,
	{noreply, State};
handle_cast({close, Socket}, State) ->
	case ets:lookup(?MODULE, Socket) of
		[{_, #established {pid = Pid, rcv_next = Ack, snd_una = Una, buf = Buf} = TcpState}] ->
			reply(Socket, Una + size(Buf), Ack, ?FIN bor ?ACK, 65535, <<>>, <<>>),
			ets:insert(?MODULE, {Socket, #fin_wait1{pid = Pid, rcv_next = Ack, snd_una = Una, buf = Buf}});
		_ -> ok
	end,
	{noreply, State}.

process_ack(Ack, #established{snd_una = Una, buf = Buf} = TcpState) ->
	AckDist = (Ack - Una) band ?SEQACK_MASK,
	if
		AckDist >= ?MAX_ACK -> TcpState; % old ACK
		AckDist =< size(Buf) -> TcpState#established{snd_una = Ack, buf = binary:part(Buf, AckDist, size(Buf) - AckDist)};
		true ->
			io:format("Ack past end of sendbuffer? snd_una = ~B, ack = ~b, buf size = ~b~n",
			          [Una, Ack, size(Buf)]),
			TcpState
	end;

process_ack(Ack, #fin_wait1{snd_una = Una, buf = Buf, pid = Pid, rcv_next = Next} = TcpState) ->
	AckDist = (Ack - Una) band ?SEQACK_MASK,
	if
		AckDist >= ?MAX_ACK -> TcpState; % old ACK
		AckDist == size(Buf) + 1 -> #fin_wait2{pid = Pid, snd_una = Ack, rcv_next = Next};
		AckDist =< size(Buf) -> TcpState#fin_wait1{snd_una = Ack, buf = binary:part(Buf, AckDist, size(Buf) - AckDist)};
		true ->
			io:format("Ack past end of sendbuffer? snd_una = ~B, ack = ~b, buf size = ~b~n",
			          [Una, Ack, size(Buf)]),
			TcpState
	end.

reply(Socket, Seq, Ack, Flags, Window, Options, Payload) when size(Payload) > 576 - 40 - size(Options) ->
	{Part1, Rest} = split_binary(Payload, 576 - 40 - size(Options)),
	reply(Socket, Seq, Ack, Flags, Window, Options, Part1),
	reply(Socket, Seq + size(Part1), Ack, Flags, Window, Options, Rest);

reply({Source, Dest, SourcePort, DestPort}, Seq, Ack, Flags, Window, Options, Payload) when size(Options) band 3 == 0 ->
	Data = (size(Options) div 4) + 5,

	PrePacket = <<DestPort:16, SourcePort:16, Seq:32, Ack:32,
		Data:4, 0:3, Flags:9, Window:16,
		0:16, 0:16, Options/binary, Payload/binary>>,

	PacketSize = size(PrePacket),
	Checksum = ip:ip_checksum(<<Dest:32, Source:32, 6:16, PacketSize:16, PrePacket/binary>>, 0),
	Packet = <<DestPort:16, SourcePort:16, Seq:32, Ack:32,
		Data:4, 0:3, Flags:9, Window:16,
		Checksum:16, 0:16, Options/binary, Payload/binary>>,
	ip:send(Dest, Source, ?TCP_PROTO, Packet).

controlling_process(Socket, Pid) ->
	case gen_server:call(?MODULE, {controlling_process, Socket, Pid}) of
		true -> redirect(Socket, Pid);
		Other ->
			io:format("tcp:controlling_process got ~w~n", [Other]),
			Other
	end.

redirect(Socket, Pid) ->
	Msg = receive
		{tcp_open, Socket, _Payload} = M -> M;
		{tcp, Socket, _Payload} = M -> M;
		{tcp_closed, Socket} = M -> M
	after 0 -> none
	end,
	case Msg of
		none -> ok;
		_ ->
			Pid ! Msg,
			redirect(Socket, Pid)
	end.

send(Socket, Data) ->
	gen_server:cast(?MODULE, {out, Socket, Data}).
close(Socket) ->
	gen_server:cast(?MODULE, {close, Socket}).
