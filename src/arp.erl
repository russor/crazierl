-module(arp).

-export([start/0]).
-behavior(gen_server).

-export ([init/1, handle_cast/2, handle_call/3]).
-export ([process/2, register/3]).

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
		[{TargetIP, #{mac := Mac, public := true}}] ->
			gen_server:call(Interface, {send, {SenderMac, 16#0806}, <<1:16, 16#0800:16, 6, 4, 2:16, Mac:48, TargetIP:32, SenderMac:48, SenderIP:32>>});
		Other ->
			io:format("arp lookup ~p~n", [Other]);
		_ -> ok
	end,
	{noreply, State}.

process(Interface, <<HType:16, PType:16, HLen:8, PLen:8, Oper:16,
		SenderHardwareAddress:(HLen*8), SenderProtocolAddress:(PLen*8),
		_TargetHardwareAddress:(HLen*8), TargetProtocolAddress:(PLen*8)>> = Packet) ->

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

handle_call({register, Pid, {A, B, C, D}, Mac}, _From, State) ->
	Ref = monitor(process, Pid),
	IP = ?u32(A, B, C, D),
	ets:insert(?MODULE, {IP, #{mac => Mac, public => true, pid => Pid}}),
	{reply, ok, State#{Ref => IP}}.

register(Pid, IP, Mac) ->
	gen_server:call(start(), {register, Pid, IP, Mac}).
