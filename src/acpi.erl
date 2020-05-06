-module (acpi).

-export ([go/0]).

go() ->
	{ok, Map} = crazierl:map(16#E0000, 32 * 4096),
	BiosMem = crazierl:bcopy_from(Map, 0, 32 * 4096),
	find_rsdp(BiosMem).


find_rsdp(<<"RSD PTR ", Rest/binary>>) ->
	<<Checksum, OEMID:6/binary, Version, RsdtAddress:32/little, _/binary>> = Rest,
	0 = checksum(<<"RSD PTR ", Checksum, OEMID:6/binary, Version, RsdtAddress:32/little>>),
	
	io:format("found checksum=~B, OEMID=~s, Version=~B, Address = ~.16B~n",
		[Checksum, OEMID, Version, RsdtAddress]),
	
	Base = RsdtAddress band (bnot 4095),
	{ok, Map} = crazierl:map(Base, 4096),
	read_table(Map, Base, RsdtAddress - Base);

find_rsdp(<<_:16/binary, Rest/binary>>) -> find_rsdp(Rest);
find_rsdp(<<>>) -> not_found.

read_table_pointers(_, _, <<>>) -> ok;
read_table_pointers(Map, Base, <<Address:32/little, Rest/binary>>) ->
	read_table(Map, Base, Address - Base),
	read_table_pointers(Map, Base, Rest).

read_table(Map, Base, Start) ->
	<<Signature:4/binary, Length:32/little>> = crazierl:bcopy_from(Map, Start, 8),
	Rest = crazierl:bcopy_from(Map, Start + 8, Length - 8),
	0 = checksum (<<Signature:4/binary, Length:32/little, Rest/binary>>),
	
	<<Revision, _Checksum,
	     OEMId:6/binary, OEMTableId:8/binary, OEMRevision:32/little,
	     CreatorId:32/little, CreatorRevision:32/little, Content/binary>> = Rest,
	
	case Signature of
		<<"RSDT">> ->
			read_table_pointers(Map, Base, Content);
		<<"APIC">> ->
			<<LocalApic:32/little, Flags:32/little, More/binary>> = Content,
			io:format("local Apic at ~.16B, Flags: ~.16B~n", [LocalApic, Flags]),
			read_apic_table(More);
		_ ->
			io:format("found ~s table, length=~B, revision=~B, "
				  "OEMID=~s, OEMTableId=~s, oemrevision=~B, creatorID=~.16B, "
				  "creatorRevision=~B, content=~2000p~n",
				  [Signature, Length, Revision, OEMId, OEMTableId, OEMRevision,
		   		   CreatorId, CreatorRevision, Content])
	end.

read_apic_table(<<>>) -> ok;
read_apic_table(<<0, 8, ProcessorId, ApicId, Flags:32/little, Rest/binary>>) ->
	io:format("Processor ~B, APIC ~B, Flags, ~B~n", [ProcessorId, ApicId, Flags]),
	read_apic_table(Rest);
read_apic_table(<<1, 12, ApicId, 0, ApicAddress:32/little, InterruptBase:32/little, Rest/binary>>) ->
	io:format("IO-APIC ~B, Address ~.16B, Interrupt Base ~.16B~n", [ApicId, ApicAddress, InterruptBase]),
	read_apic_table(Rest);
read_apic_table(<<2, 10, BusSource, IRQSource, GlobalSystemInterrupt:32/little, Flags:16/little, Rest/binary>>) ->
	io:format("ISO: BusSource ~B, IRQ Source ~B, Global Int ~B, Flags ~.16B~n", [BusSource, IRQSource, GlobalSystemInterrupt, Flags]),
	read_apic_table(Rest);
read_apic_table(<<4, 6, ProcessorId, Flags:16/little, LINT, Rest/binary>>) ->
	io:format("NMI: processor ~B, flags ~.16B, Lint ~B~n", [ProcessorId, Flags, LINT]),
	read_apic_table(Rest);
read_apic_table(<<5, 12, _:2/binary, LocalAddress:64/little, Rest/binary>>) ->
	io:format("local apic override at ~.16B~n", [LocalAddress]),
	read_apic_table(Rest).


checksum(Binary) -> checksum(0, Binary).
checksum(C, <<V, Rest/binary>>) -> checksum((C + V) band 16#ff, Rest);
checksum(C, <<>>) -> C.



