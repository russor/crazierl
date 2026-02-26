-module(gather_ws).

-include("yaws_api.hrl").

%% Export for websocket callbacks
-export([init/1, terminate/2, handle_open/2, handle_message/2]).
-export([out/1]).
-export([start/0]).

start() ->
	ets:new(?MODULE, [bag, public, named_table, {heir, group_leader(), {}}]).

out(#arg{server_path = "/ws"}) ->
	{websocket, ?MODULE, [
		{origin, "https://crazierl.org"}, % ++ ws_callback:allowed_origin(A)},
		{keepalive, true},
		{drop_on_timeout, true}
	]};
out(A = #arg{server_path = "/.well-known/acme-challenge/" ++ Token}) when Token /= []->
	case lists:all(fun is_base64url/1, Token) of
		true ->
			Filename= filename:append(filename:dirname(code:which(?MODULE)), "thumbprint.txt"),
			case file:read_file(Filename) of
				{ok, Bin} -> {content, "text/plain", [Token, ".", Bin]};
				_ -> yaws_outmod:out404(A)
			end;
		false -> yaws_outmod:out404(A)
	end;
out(A) -> yaws_outmod:out404(A).

is_base64url(C) when C >= $A, C =< $Z -> true;
is_base64url(C) when C >= $a, C =< $z -> true;
is_base64url(C) when C >= $0, C =< $9 -> true;
is_base64url($-) -> true;
is_base64url($_) -> true;
is_base64url(_) -> false.

-record(state, {wsstate, key, node}).

init([Arg, _Params]) ->
    case {yaws_api:getvar(Arg, "key"), yaws_api:getvar(Arg, "node")} of
    	{{ok, Key}, {ok, Node}} -> 
    		{ok, #state{key = Key, node = list_to_binary(Node)}};
    	_ -> {error, "missing key or node"}
    end.

handle_open(WSState, State = #state{key = Key, node = Node}) ->
    ets:insert(?MODULE, {Key, WSState, Node}),
    States = ets:lookup(?MODULE, Key),
    lists:foreach(fun 
    	({_, _, OtherNode}) when OtherNode == Node -> ok;
    	({_, OtherState, OtherNode}) -> 
    		yaws_api:websocket_send(OtherState, {binary, <<"n", Node/binary>>}),
    		yaws_api:websocket_send(WSState, {binary, <<"n", OtherNode/binary>>})
    end, States),
    {ok, State#state{wsstate = WSState}}.

handle_message({close, Status, Reason}, _) ->
    io:format("Close connection: ~p - ~p~n", [Status, Reason]),
    {close, Status}.

terminate(Reason, State = #state{key = Key, wsstate = WSState, node = Node}) ->
    ets:delete_object(?MODULE, {Key, WSState, Node}),
    io:format("terminate ~p: ~p (state:~p)~n", [self(), Reason, State]),
    ok.
