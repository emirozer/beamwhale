-module(docker).
-author("emirozer").

-export([layer_filename/1, image_dir_name/1 , h_get_token/1, get_tags/2]).


-define(DOCKER_REGISTRY, "https://registry.hub.docker.com/v1").
-define(DOCKER_HTTP_API_TOKEN_HEADER, "X-Docker-Token").
-define(BEAMWHALE_DIR, beamwhale:determine_beamwhale_dir()).

pull(Name, Tag) ->
    true.

pull(Name) ->
    true.


layer_filename(Id) ->
    ?BEAMWHALE_DIR ++ "/layers/" ++ Id ++ ".tar".

image_dir_name(Id) ->
    ?BEAMWHALE_DIR ++ "/images/" ++ Id.

save_layer(Id) ->
    Filename = layer_filename(Id),
    ChunkSize = 16 * 1024.

untar_layer(Id, Rootdir) ->
    erl_tar:extract(layer_filename(Id), {cwd, Rootdir}).

get_id(Name, Token, Tag)->
    h_get("/repositories/"+ Name +"/tags/" ++ Tag, Token).
get_id(Name, Token) ->
    h_get("/repositories/"+ Name +"/tags/latest", Token).

get_ancestry(Id, Token)->
    h_get("/images/" ++ Id ++ "/ancestry", Token).

get_tags(Name, Token)->
    httpc:request(get,
                  {?DOCKER_REGISTRY ++ "/repositories/" ++ Name ++ "/tags", 
                   [{"Authorization", "Token" ++ Token}]}, [], []).

h_get(Endpoint, Token) ->
    httpc:request(get,
                  {?DOCKER_REGISTRY ++ Endpoint, 
                   [{"Authorization", "Token" ++ Token}]}, [], []).

h_request_auth(Url, Token) ->
    httpc:request(get,
                  {Url, 
                   [{"Authorization", "Token" ++ Token}]}, [], []).
    

h_get_token(Name)->
    Url = ?DOCKER_REGISTRY ++ "/repositories/"++ Name ++"/images",
    Response = httpc:request(get, 
                             {Url, 
                              [{?DOCKER_HTTP_API_TOKEN_HEADER, "true"}]}, [], []),
    [Header | _] = [X || X <- response_headers(Response), element(1,X)==string:to_lower(?DOCKER_HTTP_API_TOKEN_HEADER)],
    extract_token_from_response_header(Header).
    

% unpacking the http resp headers
response_headers({ok, { _, Headers, _}}) -> Headers.

extract_token_from_response_header(H) ->
    [Token | _] = binary:split(lists:last(binary:split(erlang:list_to_binary(element(2,H)), <<"=">>)), <<",">>),
    erlang:binary_to_list(Token).
