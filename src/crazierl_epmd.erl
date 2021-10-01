-module(crazierl_epmd).

-include_lib("kernel/include/dist.hrl").

-export([start_link/0, names/1, register_node/2, port_please/2, port_please/3, address_please/3]).

start_link() -> ignore.
names(_) -> {ok, []}.
register_node(_, _) -> {ok, -1}.
port_please(Name, Host) -> port_please(Name, Host, infinity).
port_please(_, _, _) -> {port, 4370, ?ERL_DIST_VER_HIGH}.
address_please(Name, Host, Family) ->
	io:format("yo ~p~n", [{Name, Host, Family}]),
	{error, unimpl}.
