-module(ntp).

-define (PORT, 123).

-export([go/0]).


go() ->
	{OriginNS, ResponseNS, Response, SendOrigin} = fetch(),
	<<LeapIndicator:2, 4:3, 4:3, Stratum:8, Poll:8, Precision:8,
	  RootDelay:32, Dispersion:32, ReferenceId:4/binary,
	  ReferenceTS:8/binary, SendOrigin:8/binary, ReceiveTS:8/binary, TransmitTS:8/binary
	>>= Response,
	ReferenceNS = convert_ntp_to_nanoseconds(ReferenceTS),
	ReceiveNS = convert_ntp_to_nanoseconds(ReceiveTS),
	TransmitNS = convert_ntp_to_nanoseconds(TransmitTS),
	
	Delay = ResponseNS - OriginNS - (TransmitNS - ReceiveNS),
	Offset = (TransmitNS + ReceiveNS) div 2 - (ResponseNS + OriginNS) div 2,
	
	#{delay => Delay,
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
	  poll => Poll}.

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
	

	
		    