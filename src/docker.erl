-module(docker).
-author("emirozer").
-on_load(start_lager/0).

%% API exports
-export([pull/1, pull/2, get_tags/1, h_get_docker_auth/1]).


-define(DOCKER_REGISTRY, "https://registry-1.docker.io/v2/").
-define(DOCKER_AUTH_API, "https://auth.docker.io/token?service=registry.docker.io&scope=repository").
-define(DOCKER_REGISTRY_MANIFEST_SCHEME, "application/vnd.docker.distribution.manifest.v2+json").
-define(BEAMWHALE_DIR, beamwhale:determine_beamwhale_dir()).

-record(docker_auth, {access_token, expires_in, issued_at, token}).
%%====================================================================
%% API functions
%%====================================================================
pull(Name, Tag) ->
    lager:info("Pulling image name : ~p, with tag: ~p", [Name, Tag]),
    DockerAuth = h_get_docker_auth(Name),
    ManifestResponse = get_image_manifest(Name, DockerAuth, Tag),
    ManifestResponseCode = response_http_status(ManifestResponse),
    if
        ManifestResponseCode =/= 200 -> lager:error("Could not retrieve the manifest of the image you are looking for: ~p", [ManifestResponseCode]);
        true ->  retrieve_image(jsx:decode(
                                  list_to_binary(
                                             response_body(ManifestResponse)), [return_maps]))
    end.

pull(Name) ->
    pull(Name, "latest").

get_tags(Name)->
    DockerAuth = h_get_docker_auth(Name),
    Response = response_body(httpc:request(get,
                  {?DOCKER_REGISTRY ++ Name ++ "/tags/list", 
                   [{"Authorization", "Bearer " ++ binary_to_list(DockerAuth#docker_auth.token)}]}, [], [])),
    Map = jsx:decode(list_to_binary(Response), [return_maps]),
    lager:info("Available tags for ~p are : ~p", [Name, Map]).

%%====================================================================
%% Internal functions
%%====================================================================
retrieve_image(ManifestResponseMap) ->
    lager:info("Manifest for the image: ~p", [ManifestResponseMap]),
    %%filelib:ensure_dir(image_dir_name(Id)),
    %%Layers = get_ancestry(Id, DockerAuth),
    %%lager:info("Received ancestry : ~p", [Layers]),
    true.
    
layer_filename(Id) ->
    ?BEAMWHALE_DIR ++ "/layers/" ++ Id ++ ".tar".

image_dir_name(Id) ->
    ?BEAMWHALE_DIR ++ "/images/" ++ Id.

save_layer(Id, Name) ->
    Endpoint = "/images/" ++ Id ++ "layer",
    DockerAuth = h_get_docker_auth(Name),
    h_get(Endpoint, DockerAuth, [{stream, layer_filename(Id)}]).

untar_layer(Id, Rootdir) ->
    erl_tar:extract(layer_filename(Id), {cwd, Rootdir}).

get_image_manifest(Name, DockerAuth, Tag) ->
    h_get(Name ++"/manifests/" ++ Tag, DockerAuth, []).

h_get(Endpoint, DockerAuth, Options) ->
    lager:info("HTTP GET: ~p", [?DOCKER_REGISTRY ++ Endpoint]),
    httpc:request(get,
                  {?DOCKER_REGISTRY ++ Endpoint, 
                   [{"Authorization", "Bearer " ++ binary_to_list(DockerAuth#docker_auth.token)}, {"Accept", ?DOCKER_REGISTRY_MANIFEST_SCHEME}]}, [], Options).

h_get_docker_auth(Name)->    
    Url = ?DOCKER_AUTH_API ++":" ++ Name ++":pull",
    lager:info("Getting auth for ~p", [Url]),
    Response = response_body(httpc:request(get, 
                                           {Url, []}, [], [])),
    AuthMap = jsx:decode(list_to_binary(Response), [return_maps]),
    #docker_auth{access_token=maps:get(<<"access_token">>, AuthMap),
                 expires_in=maps:get(<<"expires_in">>, AuthMap),
                 issued_at=maps:get(<<"issued_at">>, AuthMap),
                 token=maps:get(<<"token">>, AuthMap)}.

% unpack only the http resp headers
response_headers({ok, { _, Headers, _}}) -> Headers.

% unpack only the  http resp body
response_body({ok, { _, _, Body}}) -> Body.

% unpack only the http resp status code 
response_http_status({ok, { {_, Status, _}, _, _}}) -> Status.

start_lager()->
    lager:start().
