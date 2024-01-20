-hook([cmd/1, cmd/2]).

-include_lib("kernel/src/os.erl").

hook_cmd(Cmd) -> erlang:error(system_limit, [Cmd]).
hook_cmd(Cmd, Opts) -> erlang:error(system_limit, [Cmd, Opts]).
