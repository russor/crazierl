-module(ip).

-export ([process/2, ip_checksum/2]).

% int32/u32 borrowed from dist_util.erl/inet_int.hrl
-define(int32(X),
	{((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
	 ((X) bsr 8) band 16#ff, (X) band 16#ff}).
-define(u32(X3,X2,X1,X0),
	(((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

ip_checksum(<<H:16, T/binary>>, Acc) -> ip_checksum(T, Acc + H);
ip_checksum(<<>>, Acc) when Acc > 16#FFFF ->
	ip_checksum(<<>>, (Acc bsr 16) + (Acc band 16#FFFF));
ip_checksum(<<>>, Acc) -> (bnot Acc) band 16#FFFF.

process(_Interface, <<4:4, IHL:4, DSCP:6, ECN:2, TotalLength:16, Id:16,
		     _Evil:1, DF:1, MF:1, FragmentOffset:13,
		     TTL:8, Protocol:8, HeaderChecksum:16,
		     SourceIp:32, DestIp:32, Options:(IHL * 4 - 20)/binary,
		     Payload/binary>> = Packet) ->
	ChecksumStatus = case ip_checksum(binary:part(Packet, {0, IHL * 4}), 0) of
		0 -> ok;
		Other -> Other
	end,

	case Protocol of
		_ when ChecksumStatus /= ok ->
			io:format("ip ~w -> ~w, proto ~B, ttl ~B, checksum ~.16B (~p), options (~B) ~w, payload ~w~n",
				[?int32(SourceIp), ?int32(DestIp), Protocol, TTL, HeaderChecksum, ChecksumStatus,
				 IHL, Options, Payload]);
		6 -> tcp:process(SourceIp, DestIp, TTL, Payload);
		_ ->
			io:format("ip ~w -> ~w, proto ~B, ttl ~B, checksum ~.16B, options (~B) ~w, payload ~w~n",
				[?int32(SourceIp), ?int32(DestIp), Protocol, TTL, HeaderChecksum,
				 IHL, Options, Payload])
	end.
