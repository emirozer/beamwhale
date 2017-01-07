-module(posix).
-on_load(load_posix/0).

-export([get_user/0, mount_libc/5, fork_libc/0]).

get_user() ->
    "NIF library posix not loaded".

mount_libc(Source, Target, Fs, Flags, Options) ->
    "NIF library posix not loaded".

fork_libc() ->
    "NIF library posix not loaded".

load_posix() ->
    erlang:load_nif("./src/posix", 0).
