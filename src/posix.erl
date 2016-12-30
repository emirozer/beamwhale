-module(posix).
-on_load(load_posix/0).

-export([is_user_root/0]).

is_user_root() ->
    "NIF library not loaded".

load_posix() ->
    erlang:load_nif("./posix", 0).
