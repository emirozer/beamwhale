-module(beamwhale).
-author("emirozer").


%% API exports
-export([]).

%%====================================================================
%% API functions
%%====================================================================


%%====================================================================
%% Internal functions
%%====================================================================

determine_beamwhale_dir() ->
    case posix:is_user_root() of
        1 ->
            "/var/lib/beamwhale";
        0 ->
            "~/.beamwhale"
    end.
