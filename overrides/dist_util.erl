-hook([gen_challenge/0]).
-include_lib("kernel/src/dist_util.erl").

hook_gen_challenge() ->
    binary:decode_unsigned(crypto:strong_rand_bytes(4)).

