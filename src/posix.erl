-module(posix).
-on_load(load_posix/0).

-export([get_user/0]).

get_user() ->
    "NIF library not loaded".

load_posix() ->
    erlang:load_nif("./posix", 0).
