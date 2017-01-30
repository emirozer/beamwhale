-module(beamwhale).
-author("emirozer").
-on_load(make_beamwhale/0).

%% API exports
-export([start_container/5, start_container/4, pull/1,
         pull/2, get_tags/1, determine_beamwhale_dir/0]).

-define(BEAMWHALE_DIR, determine_beamwhale_dir()).
-define(NULL, "NULL").
%%====================================================================
%% API functions
%%====================================================================

%% Options expected as a list, you can provide the following atoms
%% enable_overlay  -- uses overlayfs to build rootfs
start_container(Name, Tag, Command, Args, Options) ->
    {ok, ImageDir} = docker:pull(Name, Tag),
    {Dirname, ContainerDir} = container_dir_name(Name),
    Rootfs = ContainerDir ++ "/root/",
    filelib:ensure_dir(Rootfs),
    lager:info("Building new root filesystem in: ~p", [Rootfs]),
    CurrentUser = posix:get_user(),
    OverlayEnabled = lists:member(enable_overlay, Options),
    if
        OverlayEnabled == true andalso CurrentUser == "root" -> b_root_overlay(ImageDir, ContainerDir, Rootfs);
        OverlayEnabled == true andalso CurrentUser =/= "root" -> throw({error, root_priviliges_required_for_overlayfs});
        %% otherwise we have to do a copy op on the base image
        true -> b_root_copy_base_image(ImageDir, Rootfs)
    end,
    UserId = posix:get_user(),
    GroupId = posix:get_group_id(),
    lager:info("UserId : ~p  & GroupId: ~p", [UserId, GroupId]),
    ResMountPropogation = set_mount_propogation(),
    lager:info("set_mount_propogation result: ~p", [ResMountPropogation]),
    ResUnshare= unshare(linux:clone_newpid() bor linux:clone_newnet() bor 
                            linux:clone_newns() bor linux:clone_newuts() bor linux:clone_newcgroup() bor 
                            linux:clone_newipc() bor linux:clone_newuser()),
    lager:info("unshare syscall result: ~p", [ResUnshare]),
    setgroups_write(undefined),
    map_user(0, UserId, 1, undefined),
    map_group(0, GroupId, 1, undefined),
    posix:set_hostname(Dirname),
    
    PID = posix:fork_libc(),
    if
        PID == 0 -> do_task(Rootfs, Command, Args);
        PID =/= 0 -> wait_and_do_task(PID, Rootfs, Command, Args)
    end.

start_container(Name, Command, Args, Options) ->
    start_container(Name, "latest", Command, Args, Options).

wait_and_do_task(PID, Rootfs, Command, Args) ->
    posix:waitpid_libc(PID, 0),
    do_task(Rootfs, Command, Args).
    
do_task(Rootfs, Command, Args) ->
    setup_fs(Rootfs),
    posix:exec_libc(Command, Args).
    
pull(Name, Tag) ->
    docker:pull(Name, Tag).

pull(Name) ->
    docker:pull(Name).

get_tags(Name) ->
    docker:get_tags(Name).
%%====================================================================
%% Internal functions
%%====================================================================

determine_beamwhale_dir() ->
    CurrentUser = posix:get_user(),
    if
        CurrentUser == "root" ->
            "/var/lib/beamwhale";
        CurrentUser == "" ->
            "/tmp/beamwhale";
        true ->
            "/home/" ++ CurrentUser ++ "/.beamwhale"
    end.

make_beamwhale() ->
    file:make_dir(determine_beamwhale_dir()),
    lager:start().

container_dir_name(Name) ->
    ReplacedName = re:replace(Name, "/","-",[global,{return,list}]),
    DirectoryName = ReplacedName ++ "-" ++ uuid:to_string(uuid:uuid4()),
    {DirectoryName, determine_beamwhale_dir() ++ "/containers/" ++ DirectoryName}.

b_root_overlay(ImageDir, ContainerDir, Rootfs) ->
    lager:info("building overlayfs for container"),
    OverlayMountpoint = mount_image_overlay(ImageDir, ContainerDir, Rootfs),
    OverlayPid = posix:fork_libc(),
    if 
        OverlayPid =/= 0 -> prep_root_overlay(OverlayPid, OverlayMountpoint)
    end.

prep_root_overlay(OverlayPid, OverlayMountpoint) ->
    posix:waitpid_libc(OverlayPid, 0),
    posix:umount_libc(OverlayMountpoint),
    posix:exit_libc(0).
    
b_root_copy_base_image(ImageDir, Rootfs) ->
    os:cmd("cp -R " ++ ImageDir ++ " " ++ Rootfs).

mount_image_overlay(ImageDir, ContainerDir, Rootfs) ->
        Overlay = ContainerDir ++ "/overlay/",
        WorkDir = ContainerDir ++ "/overlay.work/",
        lists:foreach(fun(X) -> filelib:ensure_dir(X) end,[Rootfs, Overlay, WorkDir]),
        FsOptions = "lowerdir=" ++ ImageDir ++",upperdir="++ Overlay++ ",workdir=" ++ WorkDir,
        mount("none", Rootfs, "overlay", linux:ms_mgc_val(), FsOptions).


mount(Source, Target, Fs, Flags, Options) ->
    posix:mount_libc(Source, Target, Fs, Flags, Options).

unshare(Flags) ->
    posix:unshare_libc(Flags).

%% Equivalent of ->  mount --make-rprivate /
%% Prevent mounts in the container from leaking to the parent
set_mount_propogation() ->
    mount("none", "/", ?NULL, linux:ms_rec() bor linux:ms_private(), ?NULL).

%% We need to write 'deny' to /proc/PID/setgroups
%% This is needed to alter a namespace.
setgroups_write(PID) ->
    if 
        PID == undefined ->
            file:write_file(
              lists:flatten(
                io_lib:format("/proc/~B/setgroups", [posix:get_pid()])), ["deny"]);            
        true -> 
            file:write_file(
              lists:flatten(
                io_lib:format("/proc/~B/setgroups", [PID])), ["deny"])
    end.

map_user(IdInsideNs, IdOutsideNs, Length, PID) ->
    Payload = lists:flatten(io_lib:format("~B "++IdOutsideNs++" ~B", [IdInsideNs, Length])),
    lager:info("map_user Payload: ~p",[Payload]),
    if 
        PID == undefined ->
            file:write_file(
              lists:flatten(
                io_lib:format("/proc/~B/uid_map", [posix:get_pid()])), [Payload]);
        true -> 
            file:write_file(
              lists:flatten(
                io_lib:format("/proc/~B/uid_map", [PID])), [Payload])
    end.
    
map_group(IdInsideNs, IdOutsideNs, Length, PID) ->
    Payload = lists:flatten(io_lib:format("~B ~B ~B", [IdInsideNs,IdOutsideNs, Length])),
    lager:info("map_group Payload: ~p",[Payload]),
    if 
        PID == undefined ->
            file:write_file(
              lists:flatten(
                io_lib:format("/proc/~B/gid_map", [posix:get_pid()])), [Payload]);
        true -> 
            file:write_file(
              lists:flatten(
                io_lib:format("/proc/~B/gid_map", [PID])), [Payload])
    end.

pivot_root(Rootfs) ->
    mount(Rootfs, Rootfs, "bind", linux:ms_bind() bor linux:ms_rec(), ?NULL),
    OldRoot = Rootfs ++ "/.old_root",
    filelib:ensure_dir(OldRoot),
    posix:pivot(Rootfs, OldRoot),
    c:cd("/"),
    "/" ++ ".old_root".

bind_dev_nodes(OldRoot) ->
    Devices = [
      'dev/tty',
      'dev/null',
      'dev/zero',
      'dev/random',
      'dev/urandom',
      'dev/full'
     ],
    lists:foreach(fun(X) -> bind_device(X, OldRoot) end, Devices).

bind_device(Device, OldRoot) ->
    NewDevice = "/" ++ Device,
    HostDevice = OldRoot ++ "/" ++ Device,
    IsFile = filelib:is_file(NewDevice),
    if
        IsFile -> file:delete(NewDevice);
        true -> lager:debug("New device is not a file")
    end,
    mount(HostDevice, NewDevice, 'bind', linux:ms_bind(), ?NULL).

symlink_many(Mapping) ->
    maps:fold(
      fun(K, V, ok) ->
              os:cmd("ln -s " ++ K ++ " " ++ V)
      end, ok, Mapping).

setup_fs(Rootfs) ->
    OldRoot = pivot_root(Rootfs),

    %% need to mount a /proc filesystem in the namespace, if not the tools like ps and top will read
    %% from global /proc
    mount("/proc", "/proc", "proc", linux:ms_mgc_val(), ?NULL),

    %% mount a /tmpfs on /dev
    mount("/tmpfs", "/dev", "tmpfs", linux:ms_nosuid() bor linux:ms_strictatime(), ?NULL),
    
    bind_dev_nodes(OldRoot),

    %% https://www.kernel.org/doc/Documentation/filesystems/devpts.txt
    filelib:ensure_dir("/dev/pts"),
    mount("devpts", "/dev/pts", "devpts", linux:ms_noexec() bor linux:ms_nosuid(), "newinstance,ptmxmode=0666,mode=620"),

    Mapping = #{"/dev/pts/ptmx" => "/dev/ptmx", "/proc/self/fd" => "/dev/fd",
                "/proc/self/fd/0" => "/dev/stdin", "/proc/self/fd/1" => "/dev/stdout",
                "/proc/self/fd/2" => "/dev/stderr"},
    symlink_many(Mapping),

    % mount kernel /sys interface
    mount("sysfs", "/sys", "sysfs", linux:ms_rdonly() bor linux:ms_nosuid() bor
              linux:ms_noexec() bor linux:ms_nodev(), ?NULL),

   % unmount old root.
    posix:umount_libc(OldRoot),
    directory:del_dir(OldRoot).

