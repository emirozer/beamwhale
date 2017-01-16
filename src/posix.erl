-module(posix).
-on_load(load_posix/0).

-export([get_user/0, mount_libc/5, umount_libc/1,
         fork_libc/0, waitpid_libc/2, exit_libc/1,
        get_pid/0, set_hostname/1, get_group_id/0]).

get_pid() ->
    "NIF library posix not loaded".

get_user() ->
    "NIF library posix not loaded".

get_group_id() ->
    "NIF library posix not loaded".

set_hostname(Name) ->
    "NIF library posix not loaded".

mount_libc(Source, Target, Fs, Flags, Options) ->
    "NIF library posix not loaded".

umount_libc(Mountpoint) ->
    "NIF library posix not loaded".

fork_libc() ->
    "NIF library posix not loaded".

waitpid_libc(Pid, Expectation) ->
    "NIF library posix not loaded".

exit_libc(Code) ->
    "NIF library posix not loaded".

syscall_libc(SyscallCode, Flags) ->
    "NIF library posix not loaded".

load_posix() ->
    erlang:load_nif("./src/posix", 0).
