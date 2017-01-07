-module(beamwhale).
-author("emirozer").
-on_load(make_beamwhale/0).

%% API exports
-export([start_container/3, get_tags/1]).

-define(BEAMWHALE_DIR, determine_beamwhale_dir()).

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
        OverlayEnabled and CurrentUser =/= "root" -> {error, root_priviliges_required_for_overlayfs};
        %% otherwise we have to do a copy op on the base image
        true -> b_root_copy_base_image(ImageDir, Rootfs)
    end.
    
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
    true.

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
