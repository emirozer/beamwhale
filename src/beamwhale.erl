-module(beamwhale).
-author("emirozer").


%% API exports
-export([determine_beamwhale_dir/0, container_dir_name/1]).

%%====================================================================
%% API functions
%%====================================================================


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
            "/home/" ++ posix:get_user() ++ "/.beamwhale"
    end.

make_beamwhale_dir() ->
    file:make_dir(determine_beamwhale_dir()).

container_dir_name(Name) ->
    DirectoryName = Name ++ "-" ++ uuid:to_string(uuid:uuid4()),
    {DirectoryName, determine_beamwhale_dir() ++ "/containers/" ++ DirectoryName}.
