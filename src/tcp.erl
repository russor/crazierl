-module(tcp).

-export ([process/4]).

% int32/u32 borrowed from dist_util.erl/inet_int.hrl
-define(int32(X),
	{((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
	 ((X) bsr 8) band 16#ff, (X) band 16#ff}).
-define(u32(X3,X2,X1,X0),
	(((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

process(Source, Dest, TTL, <<SourcePort:16, DestPort:16, Sequence:32, Ack:32,
			     Data:4, 0:3, NS:1, CWR:1, ECE:1, URG:1, ACK:1, PSH:1, RST:1, SYN:1, FIN:1,
			     WindowSize:16, CheckSum:16, UrgPTR:16,
			     Options:(Data * 4 - 20)/binary, Payload/binary>> = Packet) ->
	PacketSize = size(Packet),
	ChecksumStatus = case ip:ip_checksum(<<Source:32, Dest:32, 6:16, PacketSize:16, Packet/binary>>, 0) of
		0 -> ok;
		Other -> Other
	end,

	io:format("tcp ~w:~B -> ~w:~B, ttl ~B, checksum ~.16B (~p), options (~w) ~w, payload ~w~n",
		[?int32(Source), SourcePort, ?int32(Dest), DestPort, TTL, CheckSum, ChecksumStatus,
		 Options, Data, Payload]).
