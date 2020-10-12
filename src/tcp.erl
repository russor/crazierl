-module(tcp).

-export([start/0]).
-behavior(gen_server).

-export ([init/1, handle_cast/2, handle_call/3]).

-export ([listen/2]).
-export ([process/4]).
-export ([format_packet/4]).

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
	          "checksum ~.16B (~p), options (~w) ~w, payload ~w~n",
		[?int32(Source), SourcePort, ?int32(Dest), DestPort, FlagField, Seq, Ack, Window, UrgPtr,
		TTL, CheckSum, ChecksumStatus, Options, Data * 4 - 20, Payload]).

format_flags(<<1:1, Flags/bitstring>>, <<C, Chars/binary>>, Acc) ->
	format_flags(Flags, Chars, [C|Acc]);
format_flags(<<0:1, Flags/bitstring>>, <<_, Chars/binary>>, Acc) ->
	format_flags(Flags, Chars, Acc);
format_flags(<<>>, <<>>, []) -> "none";
format_flags(<<>>, <<>>, Acc) -> Acc.

handle_call({listen, Port, Pid}, _From, State) ->
	Reply = ets:insert_new(?MODULE, {Port, listen, Pid}),
	{reply, Reply, State}.

% check ack only first
handle_cast({in, {Source, Dest, SourcePort, DestPort, Seq, Ack, ?ACK, Window, UrgPtr, Options, Payload}}, State) ->
	case ets:lookup(?MODULE, {Source, Dest, SourcePort, DestPort}) of
		%[{_, established, ....] -> % fast path established, ACK only, right sequence, same window
		[] -> io:format("should send reset for ACK~n")
	end,
	{noreply, State};

% SYN
handle_cast({in, {Source, Dest, SourcePort, DestPort, Seq, 0 = Ack, ?SYN, Window, UrgPtr, Options, Payload}}, State) ->
	Key = {Source, Dest, SourcePort, DestPort},
	case ets:lookup(?MODULE, Key) of
		[{_, syn_received, Seq, MyISS}] ->
			reply(Key, MyISS, Seq + 1, ?SYN bor ?ACK, 65535, <<>>, <<>>);
		[Other] ->
			io:format("ignoring syn on connection ~w~n", [Other]);
		[] ->
			case ets:lookup(?MODULE, DestPort) of
				[{_, listen, Pid}] ->
					MyISS = rand:uniform(1000), % FIXME should be crypto random
					ets:insert(?MODULE, {Key, syn_received, Seq, MyISS}),
					reply(Key, MyISS, Seq + 1, ?SYN bor ?ACK, 65535, <<>>, <<>>);
				[] ->
					reply(Key, 0, Seq + 1, ?RST bor ?ACK, 0, <<>>, <<>>)
			end
	end,
	{noreply, State}.

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
