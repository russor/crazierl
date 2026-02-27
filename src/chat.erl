-module (chat).
-export ([start/0, init/0, start_impl/1, loop/1, input_loop/2]).

%%%%%%%%%%%%%%%%%
% (dist)chat
%
% start pg (-kernel start_pg true) or otherwise
% chat:start()
% Commands:
%   /quit 	: leave
%   /nick Nick  : change your nickname to Nick
%
% You can also run a node intended only for chat with something like
%    erl -sname node1 -kernel start_pg true -noshell -run chat
% or
%    erl -sname node2 -kernel start_pg true -noshell -run chat
%
% (node1 will net_adm:ping to node2, all other nodes will ping to node1)

-define(GROUP, ?MODULE).
-record(state, {self, monref, user, known, pending, input_ref, input, from_init}).

start() when node() == 'nonode@nohost' ->
	{error, dist_required};
start() ->
	case shell:start_interactive({?MODULE, init, []}) of
		{error, already_started} -> start_in_shell();
		Other -> Other
	end.

init() ->
	process_flag(trap_exit, true),
	spawn_link(?MODULE, start_impl, [true]).

start_in_shell() ->
	OldTrap = process_flag(trap_exit, true),
	Pid = spawn_link(?MODULE, start_impl, [false]),
	Ret = receive
		{'EXIT', Pid, normal} -> ok;
		{'EXIT', Pid, Other} -> {error, Other}
	end,
	process_flag(trap_exit, OldTrap),
	Ret.

start_impl(FromInit) ->
	User = os:getenv("USER", "unknown"),
	io:setopts([binary]),
	io:format("starting distchat~n"),
	
	case FromInit of
		true ->
			{node, Name, Host} = dist_util:split_node(node()),
			OtherNode = case Name of
				"node1" -> "node2";
				_ -> "node1"
			end,
			_ = net_adm:ping(list_to_atom(OtherNode ++ "@" ++ Host));
		_ -> ok
	end,
		
	ok = pg:join(?GROUP, self()),
	{MonRef, Pids} = pg:monitor(?GROUP),
	
	hello(self(), User, Pids),
	InputRef = make_ref(),
	Input = spawn_link(?MODULE, input_loop, [self(), InputRef]),
	?MODULE:loop(#state{
		self = self(),
		monref = MonRef,
		user = User,
		known = #{},
		pending = maps:from_keys(Pids, true),
		input_ref = InputRef,
		input = Input,
		from_init = FromInit
		
	}).
	
loop(State = #state{self = Self, monref = MonRef, user = User, known = Known, pending = Pending, input = Input, input_ref = InputRef, from_init = FromInit}) ->
	receive
		{hello, Pid, Name} ->
			WasPending = maps:get(Pid, Pending, false),
			OldName = maps:get(Pid, Known, none),
			case {WasPending, OldName} of
				{true, _} when Pid == Self ->
					io:format("*** You (~s) have joined (~s)~n", [Name, node(Pid)]);
				{true, _} ->
					io:format("*** ~s has joined (~s)~n", [Name, node(Pid)]);
				{false, none} ->
					receive
						{MonRef, join, ?GROUP, [Pid]} ->
							hello(Self, User, [Pid]),
							io:format("*** ~s has joined (~s)~n", [Name, node(Pid)])
					after 10 ->
						io:format("*** ~s has joined, but wasn't pending?? (~s) (~p)~n", [Name, node(Pid), Pid])
					end;
				{false, _} when Pid == Self ->
					io:format("*** You (~s) are now known as ~s (~s)~n", [OldName, Name, node(Pid)]);
				{false, _} ->
					io:format("*** ~s is now known as ~s (~s)~n", [OldName, Name, node(Pid)])
			end,
			?MODULE:loop(State#state{known = maps:put(Pid, Name, Known), pending = maps:remove(Pid, Pending)});
		{MonRef, join, ?GROUP, Pids} ->
			hello(Self, User, Pids),
			?MODULE:loop(State#state{pending = maps:merge(Pending, maps:from_keys(Pids, true))});
		{MonRef, leave, ?GROUP, Pids} ->
			{NewKnown, NewPending} = lists:foldl(fun(Pid, {K, P}) ->
				Name = maps:get(Pid, Known, "<unknown>"),
				io:format("*** ~s has left (~s)~n", [Name, node(Pid)]),
				{maps:remove(Pid, K), maps:remove(Pid, P)}
			end, {Known, Pending}, Pids),
			?MODULE:loop(State#state{known = NewKnown, pending = NewPending});
		{InputRef, <<"/", WholeCommand/binary>>} ->
			[Command | Args] = string:split(string:chomp(WholeCommand), " "),
			NewState = case string:casefold(Command) of
				<<"quit">> -> case FromInit of
					true -> erlang:halt();
					_ -> exit(normal)
				end;
				<<"nick">> when length(Args) == 1 ->
					NewUser = hd(Args),
					hello(Self, NewUser, maps:keys(Known)),
					hello(Self, NewUser, maps:keys(Pending)),
					State#state{user= NewUser};
				_ ->
					io:format("*** unknown command ~s~n", [Command]),
					State
			end,
			Input ! ok,
			?MODULE:loop(NewState);
		{InputRef, Message} ->
			Chomped = string:chomp(Message),
			maps:foreach(fun(Pid, _Name) when Pid == Self -> ok ;
				        (Pid, _Name) ->
				Pid ! { message, Self, Chomped }
			end, Known),
			Input ! ok,
			?MODULE:loop(State);
		{message, Pid, Message} ->
			Name = maps:get(Pid, Known, "<unknown>"),
			% this format is "Ye Olde ARPAnet Kludge"
			io:format("<~s%~s> ~s~n", [Name, node(Pid), Message]),
			?MODULE:loop(State);
		OtherMsg ->
			io:format("unknown message ~p~n", [OtherMsg]),
			?MODULE:loop(State)
	end.

hello(_, _, []) -> ok;
hello(Self, User, [Pid | Rest]) ->
	Pid ! {hello, Self, User},
	hello(Self, User, Rest).

input_loop(Parent, Ref) ->
	Line = io:get_line(<<"> ">>),
	Parent ! {Ref, Line},
	receive
		ok -> ok
	end,
	input_loop(Parent, Ref).
