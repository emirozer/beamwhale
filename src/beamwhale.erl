-module(beamwhale).
-author("emirozer").


%% API exports
-export([determine_beamwhale_dir/0]).

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
        true ->
            "/home/" ++ posix:get_user() ++ "/.beamwhale"
    end.


make_beamwhale_dir() ->
    file:make_dir(determine_beamwhale_dir()).

container_dir_name(name) ->
    true.
