-module(beamwhale).
-author("emirozer").
-on_load(make_beamwhale/0).

%% API exports
-export([start_container/3, pull/1, pull/2, get_tags/1]).

-define(BEAMWHALE_DIR, determine_beamwhale_dir()).
-define(NULL, "NULL").
%%====================================================================
%% API functions
%%====================================================================

%% Options expected as a list, you can provide the following atoms
%% enable_overlay  -- uses overlayfs to build rootfs
start_container(Name, Tag, Options) ->
    {ok, ImageDir} = docker:pull(Name, Tag),
    {Dirname, ContainerDir} = container_dir_name(Name),
    Rootfs = ContainerDir ++ "/root/",
    filelib:ensure_dir(Rootfs),
    lager:info("Building new root filesystem in: ~p", [Rootfs]),
    CurrentUser = posix:get_user(),
    OverlayEnabled = lists:member(enable_overlay, Options),
    if
        OverlayEnabled and CurrentUser == "root" -> b_root_overlay(ImageDir, ContainerDir, Rootfs);
        OverlayEnabled and CurrentUser =/= "root" ->throw({error, root_priviliges_required_for_overlayfs});
        %% otherwise we have to do a copy op on the base image
        true -> b_root_copy_base_image(ImageDir, Rootfs)
    end,
    UserId = posix:get_user(),
    GroupId = posix:get_group_id(),
    
    unshare(linux:clone_newpid() bor linux:clone_newnet() bor linux:clone_newns() bor
           linux:clone_newuts() bor linux:clone_newcgroup() bor linux:clone_newipc() bor
                linux:clone_newuser()),
    set_mount_propogation(),
    setgroups_write(undefined),
    map_user(0, UserId, 1, undefined),
    map_group(0, GroupId, 1, undefined),
    posix:set_hostname(Dirname).
    


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
    file:copy(ImageDir, Rootfs).

mount_image_overlay(ImageDir, ContainerDir, Rootfs) ->
        Overlay = ContainerDir ++ "/overlay/",
        WorkDir = ContainerDir ++ "/overlay.work/",
        lists:foreach(fun(X) -> filelib:ensure_dir(X) end,[Rootfs, Overlay, WorkDir]),
        FsOptions = "lowerdir=" ++ ImageDir ++",upperdir="++ Overlay++ ",workdir=" ++ WorkDir,
        mount("none", Rootfs, "overlay", linux:ms_mgc_val(), FsOptions).


mount(Source, Target, Fs, Flags, Options) ->
    posix:mount_libc(Source, Target, Fs, Flags, Options).

unshare(Flags) ->
    posix:syscall_libc(linux:sys_unshare(), Flags).

%% Equivalent of ->  mount --make-rprivate /
%% Prevent mounts in the container from leaking to the parent
set_mount_propogation() ->
    mount("none", "/", ?NULL, linux:ms_rec() bor linux:ms_private(), ?NULL).

%% We need to write 'deny' to /proc/PID/setgroups
%% This is needed to alter a namespace.
setgroups_write(PID) ->
    if 
        PID == undefined ->
            file:write_file("/proc/" ++ posix:get_pid() ++ "/setgroups", ["deny"]);            
        true -> 
            file:write_file("/proc/" ++ PID ++ "/setgroups", ["deny"])
    end.

map_user(IdInsideNs, IdOutsideNs, Length, PID) ->
    Payload = IdInsideNs ++ " " ++ IdOutsideNs ++ " " ++ Length,
    if 
        PID == undefined ->
            file:write_file("/proc/" ++ posix:get_pid() ++ "/uid_map", [Payload]);
        true -> 
            file:write_file("/proc/" ++ PID ++ "/uid_map", [Payload])
    end.
    
map_group(IdInsideNs, IdOutsideNs, Length, PID) ->
    Payload = IdInsideNs ++ " " ++ IdOutsideNs ++ " " ++ Length,
    if 
        PID == undefined ->
            file:write_file("/proc/" ++ posix:get_pid() ++ "/gid_map", [Payload]);
        true -> 
            file:write_file("/proc/" ++ PID ++ "/gid_map", [Payload])
    end.

