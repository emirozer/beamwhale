-module(linux).

-compile(export_all).

sys_unshare() -> % kernel/fork.c
    272.


%% linux/sched.h
clone_vm() -> hex_to_int(<<"0x00000100">>).  % set if VM shared between processes
clone_fs() -> hex_to_int(<<"0x00000200">>).  % set if fs info shared between processes
clone_files() -> hex_to_int(<<"0x00000400">>).  % set if open files shared between processes
clone_sighand() -> hex_to_int(<<"0x00000800">>).  % set if signal handlers and blocked signals shared
clone_ptrace() ->  hex_to_int(<<"0x00002000">>).  % set if we want to let tracing continue on the child too
clone_vfork() ->  hex_to_int(<<"0x00004000">>).  % set if the parent wants the child to wake it up on mm_release
clone_parent() -> hex_to_int(<<"0x00008000">>).  % set if we want to have the same parent as the cloner
clone_thread() -> hex_to_int(<<"0x00010000">>).  % Same thread group?
clone_newns() -> hex_to_int(<<"0x00020000">>).  % New mount namespace group
clone_sysvsem() -> hex_to_int(<<"0x00040000">>).  % share system V SEM_UNDO semantics
clone_settls() -> hex_to_int(<<"0x00080000">>).  % create a new TLS for the child
clone_parent_settid() -> hex_to_int(<<"0x00100000">>).  % set the TID in the parent
clone_child_cleartid() -> hex_to_int(<<"0x00200000">>).  % clear the TID in the child
clone_detached() -> hex_to_int(<<"0x00400000">>).  % Unused, ignored
clone_untraced() -> hex_to_int(<<"0x00800000">>).  % set if the tracing process can't force CLONE_PTRACE on this clone
clone_child_settid() -> hex_to_int(<<"0x01000000">>).  % set the TID in the child
clone_newcgroup() -> hex_to_int(<<"0x02000000">>).  % New cgroup namespace
clone_newuts() -> hex_to_int(<<"0x04000000">>).  % New utsname namespace
clone_newipc() -> hex_to_int(<<"0x08000000">>).  % New ipc namespace
clone_newuser() -> hex_to_int(<<"0x10000000">>).  % New user namespace
clone_newpid() ->  hex_to_int(<<"0x20000000">>).  % New pid namespace
clone_newnet() ->  hex_to_int(<<"0x40000000">>).  % New network namespace
clone_io() -> hex_to_int(<<"0x80000000">>).  % Clone io context


%% linux/fs.h
ms_rdonly() -> 1. % Mount read-only
ms_nosuid() -> 2. % Ignore suid and sgid bits
ms_nodev() ->  4. % Disallow access to device special files
ms_noexec() -> 8. % Disallow program execution
ms_synchronous() -> 16. % Writes are synced at once
ms_remount() -> 32. % Alter flags of a mounted FS
ms_mandlock() -> 64. % Allow mandatory locks on an FS
ms_dirsync() -> 128. % Directory modifications are synchronous
ms_noatime() -> 1024.% Do not update access times.
ms_nodiratime() -> 2048.% Do not update directory access times
ms_bind() -> 4096.
ms_move() -> 8192.
ms_rec() -> 16384.
ms_silent() -> 32768.
ms_posixacl() -> (1 bsl 16). % VFS does not apply the umask
ms_unbindable() -> (1 bsl 17). % change to unbindable
ms_private() -> (1 bsl 18). % change to private
ms_strictatime() -> (1 bsl 24). % Always perform atime updates
ms_mgc_val() -> hex_to_int(<<"0xC0ED0000">>).% Old magic mount flag

mnt_force() -> hex_to_int(<<"0x00000001">>).% Attempt to forcibily umount
mnt_detach() -> hex_to_int(<<"0x00000002">>).% Just detach from the tree
mnt_expire() -> hex_to_int(<<"0x00000004">>).% Mark for expiry
umount_nofollow() -> hex_to_int(<<"0x00000008">>).% Don't follow symlink on umount
umount_unused() -> hex_to_int(<<"0x80000000">>).% Flag guaranteed to be unused

hex_to_int(Hex) ->
    erlang:binary_to_integer(erlang:list_to_binary(lists:subtract(erlang:binary_to_list(Hex),"0x")), 16).
