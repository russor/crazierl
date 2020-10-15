-module(arp).

-export([start/0]).
-behavior(gen_server).

-export ([init/1, handle_cast/2, handle_call/3]).
-export ([process/2, register/5]).
-export ([send/3]).

% int32/u32 borrowed from dist_util.erl/inet_int.hrl
-define(int32(X),
        {((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
         ((X) bsr 8) band 16#ff, (X) band 16#ff}).
-define(u32(X3,X2,X1,X0),
        (((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

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

handle_cast({request, Interface, TargetIP, SenderMac, SenderIP}, State) ->
	case ets:lookup(?MODULE, TargetIP) of
		[{_, #{mac := Mac, public := true}}] ->
			gen_server:call(Interface, {send, {SenderMac, 16#0806}, <<1:16, 16#0800:16, 6, 4, 2:16, Mac:48, TargetIP:32, SenderMac:48, SenderIP:32>>});
		Other ->
			io:format("arp lookup ~p~n", [Other]);
		_ -> ok
	end,
	{noreply, State};
handle_cast({send, Interface, SourceIP, SourceMac, DestIP, Packet}, State) ->
	case ets:lookup(?MODULE, DestIP) of % relookup, avoid race condition
		[{_, #{mac := DestMac}}] ->
			gen_server:call(Interface, {send, {DestMac, 16#0800}, Packet});
		[] ->
			gen_server:call(Interface, {send, {16#FFFFFF_FFFFFF, 16#0806}, <<1:16, 16#0800:16, 6, 4, 1:16, SourceMac:48, SourceIP:32, 0:48, DestIP:32>>}),
			io:format("dropping packet from ~p to ~p, no arping buffer~n", [?int32(SourceIP), ?int32(DestIP)])
	end,
	{noreply, State};
handle_cast({reply, _Interface, SourceMac, SourceIP}, State) ->
	ets:insert_new(?MODULE, {SourceIP, #{mac => SourceMac}}),
	{noreply, State}.

process(Interface, <<HType:16, PType:16, HLen:8, PLen:8, Oper:16,
		SenderHardwareAddress:(HLen*8), SenderProtocolAddress:(PLen*8),
		_TargetHardwareAddress:(HLen*8), TargetProtocolAddress:(PLen*8), Padding/binary>> = Packet) ->

	case {HType, PType, HLen, PLen} of
		{1, 16#800, 6, 4} ->
			case Oper of
				1 ->
					io:format("arp Request who-has ~p tell ~p~n",
					          [?int32(TargetProtocolAddress), ?int32(SenderProtocolAddress)]),
					gen_server:cast(?MODULE, {request, Interface, TargetProtocolAddress, SenderHardwareAddress, SenderProtocolAddress});
				2 ->
					io:format("arp Reply ~p is at ~12.16.0B tell ~p~n",
					          [?int32(SenderProtocolAddress), SenderHardwareAddress, ?int32(TargetProtocolAddress)]),
					gen_server:cast(?MODULE, {reply, Interface, SenderHardwareAddress, SenderProtocolAddress});
				_ ->
					io:format("unexpected arp operation ~B, ~w on ~w ~n", [Oper, Packet, Interface])
			end;
		_ ->
			io:format("unexpected arp types, ~w on ~w ~n", [Packet, Interface])
	end.

handle_call({register, Pid, {A, B, C, D}, Mask, {E, F, G, H}, Mac}, _From, State) ->
	Ref = monitor(process, Pid),
	IP = ?u32(A, B, C, D),
	RouterIP = ?u32(E, F, G, H),
	RealMask = (1 bsl (32 - Mask)) - 1,
	ets:insert(?MODULE, {IP, #{mac => Mac, public => true, pid => Pid, mask => RealMask, router => RouterIP}}),
	{reply, ok, State#{Ref => IP}}.

register(Pid, IP, Mask, Router, Mac) ->
	gen_server:call(start(), {register, Pid, IP, Mask, Router, Mac}).

send(Source, Dest, Packet) ->
	case ets:lookup(?MODULE, Source) of
		[{_, #{mac := SourceMac, pid := Interface, mask := Mask, router := Router}}] ->
			EtherDest = if
				Source band Mask == Dest band Mask -> Dest;
				true -> Router
			end,

			case ets:lookup(?MODULE, EtherDest) of
				[{_, #{mac := DestMac}}] ->
					gen_server:call(Interface, {send, {DestMac, 16#0800}, Packet});
				[] ->
					gen_server:cast(?MODULE, {send, Interface, Source, SourceMac, EtherDest, Packet})
			end;
		_ -> io:format("can't send packet from ~p to ~p, no source mac on file~n", [?int32(Source), ?int32(Dest)])
	end.
