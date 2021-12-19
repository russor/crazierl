-module(ntp).

-define (PORT, 123).

-export([go/0]).


go() ->
	List = go([], 8, 2000),
	Now = erlang:convert_time_unit(os:system_time(), native, nanosecond),
	RelativeList = lists:map(fun ({T, O}) -> {T - Now, O} end, List),
	{SumX, SumY, SumXX, SumXY, N} = sums(RelativeList, 0, 0, 0, 0, 0),

% https://www.mathsisfun.com/data/least-squares-regression.html
% m = (N Σ(xy) − Σx Σy) / N Σ(x * x) − (Σx)2
% b = (Σy − m Σx) / N

	Frequency = (N * SumXY - SumX * SumY) / (N * SumXX - SumX * SumX),
	Offset = (SumY - Frequency * SumX) / N,
	io:format("offset = ~f x + ~f~n", [Frequency * 1_000_000, Offset]),
	MicroSeconds = round(Offset) div 1_000,
	Seconds = MicroSeconds div 1_000_000,
	MicroSeconds2 = MicroSeconds rem 1_000_000,
	io:format("~B seconds, ~B us~n", [Seconds, MicroSeconds2]),
	ok = crazierl:time_offset(Seconds, MicroSeconds2),
	ScaledFrequency = round(Frequency * 1_000_000 * (1 bsl 16)),
	io:format("Scaled PPM ~B~n", [ScaledFrequency]),
	ok = crazierl:ntp_adjtime_freq(ScaledFrequency),
	RelativeList.

sums([], X, Y, XX, XY, N) -> {X, Y, XX, XY, N};
sums([{T, O} | Tail], X, Y, XX, XY, N) ->
	sums(Tail, X + T, Y + O, XX + (T * T), XY + (T * O), N +1).

go(Acc, Count, Wait) ->
	{Acc1, Wait1} = case fetch() of
	{OriginNS, ResponseNS, Response, SendOrigin} ->
		<<LeapIndicator:2, 4:3, 4:3, Stratum:8, Poll:8, Precision:8,
		  RootDelay:32, Dispersion:32, ReferenceId:4/binary,
		  ReferenceTS:8/binary, SendOrigin:8/binary, ReceiveTS:8/binary, TransmitTS:8/binary
		>>= Response,
		ReferenceNS = convert_ntp_to_nanoseconds(ReferenceTS),
		ReceiveNS = convert_ntp_to_nanoseconds(ReceiveTS),
		TransmitNS = convert_ntp_to_nanoseconds(TransmitTS),

		Delay = ResponseNS - OriginNS - (TransmitNS - ReceiveNS),
		Offset = (TransmitNS + ReceiveNS) div 2 - (ResponseNS + OriginNS) div 2,

		case Stratum of
			0 when ReferenceId == <<"RATE">> ->
				io:format("rate limited, doubling wait~n", []),
				{Acc, Wait * 2};
			0 ->
				io:format("got ~p~n",
					[#{delay => Delay,
					  offset => Offset,
					  originNs => OriginNS,
					  responseNs => ResponseNS,
					  leap => LeapIndicator,
					  stratum => Stratum,
					  precision => Precision,
					  rootDelay => RootDelay,
					  dispersion => Dispersion,
					  referenceId => ReferenceId,
					  referenceNS => ReferenceNS,
					  receiveNS => ReceiveNS,
					  transmitNS => TransmitNS,
					  poll => Poll}]),
				{Acc, Wait};
			_ ->
				{[{OriginNS + (ResponseNS - OriginNS) div 2, Offset} | Acc], Wait}
		end;
	timeout -> {Acc, Wait * 2}
	end,
	
	case Count of
		1 -> Acc1;
		_ ->
			timer:sleep(Wait1),
			go(Acc1, Count - 1, Wait1)
	end.

convert_ntp_to_nanoseconds(<<Seconds:32, Fraction:32>>) ->
	UnixSeconds = Seconds - 2_208_988_800, % RFC 5905 Figure 4
	FractionNanos = erlang:convert_time_unit(Fraction, second, nanosecond) div 16#1_0000_0000,
	erlang:convert_time_unit(UnixSeconds, second, nanosecond) + FractionNanos.

fetch() ->
	{ok, Port} = gen_udp:open(0, [binary]),
	
	{OriginTS, SendOrigin} = {os:system_time(), crypto:strong_rand_bytes(8)},
	
	ok = gen_udp:send(Port, "192.168.0.10", ?PORT,
		<< 0:2, 4:3, 3:3, 0:8, % no leap info, version 4, client, no stratum
		   0:8, 0:8, 0:32, 0:32, % no poll, precision, root delay, dispersion
		   0:32, 0:64, % no reference ID, timestamp
		   0:64, 0:64, % no origin or receive timestamp
		   SendOrigin/binary>>), % send random transmit timestamp, no extensions
	R = receive
		{udp, Port, _Ip, ?PORT, Response} ->
			ResponseTS = os:system_time(),
			{erlang:convert_time_unit(OriginTS, native, nanosecond), 
			 erlang:convert_time_unit(ResponseTS, native, nanosecond),
			 Response, SendOrigin}
		after 2000 -> timeout
	end,
	gen_udp:close(Port),
	R.
