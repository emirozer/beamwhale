-module(docker).
-author("emirozer").
-on_load(start_lager/0).

%% API exports
-export([pull/1, pull/2, get_tags/1]).


-define(DOCKER_REGISTRY, "https://registry.hub.docker.com/v1").
-define(DOCKER_HTTP_API_TOKEN_HEADER, "X-Docker-Token").
-define(BEAMWHALE_DIR, beamwhale:determine_beamwhale_dir()).

%%====================================================================
%% API functions
%%====================================================================
pull(Name, Tag) ->
    lager:info("Pulling image name : ~p, with tag: ~p", [Name, Tag]),
    Token = h_get_token(Name),
    Id = get_image_id(Name, Token, Tag),
    lager:info("Id is : ~p", [Id]),
    filelib:ensure_dir(image_dir_name(Id)),
    Layers = get_ancestry(Id, Token),
    lager:info("Received ancestry : ~p", [Layers]),
    true.

pull(Name) ->
    pull(Name, "latest").

get_tags(Name)->
    Token = h_get_token(Name),
    response_body(httpc:request(get,
                  {?DOCKER_REGISTRY ++ "/repositories/" ++ Name ++ "/tags", 
                   [{"Authorization", "Token" ++ Token}]}, [], [])).

%%====================================================================
%% Internal functions
%%====================================================================
layer_filename(Id) ->
    ?BEAMWHALE_DIR ++ "/layers/" ++ Id ++ ".tar".

image_dir_name(Id) ->
    ?BEAMWHALE_DIR ++ "/images/" ++ Id.

save_layer(Id, Name) ->
    Endpoint = "/images/" ++ Id ++ "layer",
    Token = h_get_token(Name),
    h_request_auth(?DOCKER_REGISTRY ++ Endpoint, Token, [{stream, layer_filename(Id)}]).

untar_layer(Id, Rootdir) ->
    erl_tar:extract(layer_filename(Id), {cwd, Rootdir}).

get_image_id(Name, Token, Tag)->
    h_get("/repositories/"++ Name ++"/tags/" ++ Tag, Token).

get_ancestry(Id, Token)->
    h_get("/images/" ++ Id ++ "/ancestry", Token).

h_get(Endpoint, Token) ->
    httpc:request(get,
                  {?DOCKER_REGISTRY ++ Endpoint, 
                   [{"Authorization", "Token" ++ Token}]}, [], []).

h_request_auth(Url, Token) ->
    h_request_auth(Url, Token, []).
h_request_auth(Url, Token, Options) ->
    httpc:request(get,
                  {Url, 
                   [{"Authorization", "Token" ++ Token}]}, [], Options).
    

h_get_token(Name)->
    Url = ?DOCKER_REGISTRY ++ "/repositories/"++ Name ++"/images",
    Response = httpc:request(get, 
                             {Url, 
                              [{?DOCKER_HTTP_API_TOKEN_HEADER, "true"}]}, [], []),
    [Header | _] = [X || X <- response_headers(Response), element(1,X)==string:to_lower(?DOCKER_HTTP_API_TOKEN_HEADER)],
    extract_token_from_response_header(Header).
    

% unpack only the http resp headers
response_headers({ok, { _, Headers, _}}) -> Headers.

% unpack only the  http resp body
response_body({ok, { _, _, Body}}) -> Body.

extract_token_from_response_header(H) ->
    [Token | _] = binary:split(lists:last(binary:split(erlang:list_to_binary(element(2,H)), <<"=">>)), <<",">>),
    erlang:binary_to_list(Token).

start_lager()->
    lager:start().
