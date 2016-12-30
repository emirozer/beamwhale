-module(docker).
-author("emirozer").

-export([layer_filename/1, image_dir_name/1 , h_get_token/1, get_tags/2]).


-define(DOCKER_REGISTRY, "https://registry.hub.docker.com/v1").
-define(DOCKER_HTTP_API_TOKEN_HEADER, "X-Docker-Token").
-define(BEAMWHALE_DIR, beamwhale:determine_beamwhale_dir()).

pull(Name, Tag) ->
    true;
pull(Name, _) ->
    true.


layer_filename(Id) ->
    ?BEAMWHALE_DIR ++ "/layers/" ++ Id ++ ".tar".

image_dir_name(Id) ->
    ?BEAMWHALE_DIR ++ "/images/" ++ Id.

save_layer(Id) ->
    true.

untar_layer(Id, Rootdir) ->
    true.

get_id(Name, Tag)->
    true;
get_id(Name, _) ->
    true.

get_ancestry(Id)->
    true.

get_tags(Name, Token)->
    Response = httpc:request(get,
                             {?DOCKER_REGISTRY ++ "/repositories/" ++ Name ++ "/tags", 
                              [{"Authorization", "Token" ++ Token}]}, [], []).

h_get(Endpoint) ->
    true.

h_request_auth(Url, Token) ->
    Response = httpc:request(get,
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
