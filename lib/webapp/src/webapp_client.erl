% -*- fill-column: 100; -*-

-module(webapp_client).
-export([run_once/0, init_httpc/0, http_get/1, http_get/2, http_post/2, http_post/3,
         http_multipart_post/2, bespoke_cookie/1]).

-include_lib("apptools/include/shorthand.hrl").

%%
%% Exported: run_once
%%

-spec run_once() -> ok.

run_once() ->
    db_tools:create_subreddit_db().

%%
%% Exported: init_httpc
%%

-spec init_httpc() -> ok | {error, term()}.

init_httpc() ->
    application:start(inets),
    inets:start(httpc, [{profile, poop}]).

%%
%% Exported: http_get
%%

-spec http_get(string()) ->
          {ok, term()} |
          {unexpected, integer(), string(), list(), iolist()} |
          {error, term()}.

http_get(Url) ->
    http_get(Url, []).

http_get(Url, Headers) ->
    handle_response(httpc:request(get, {Url, [{"connection", "close"}|Headers]}, [], [])).

%%
%% Exported: http_post
%%

-spec http_post(string(), term()) ->
          {ok, term()} |
          {unexpected, integer(), string(), list(), iolist()} |
          {error, term()}.

http_post(Url, Data) ->
    http_post(Url, Data, []).

http_post(Url, Data, Headers) ->
    handle_response(
      httpc:request(post, {Url, [{"connection", "close"}|Headers],
                           "application/json", json:encode(Data)}, [], [])).

%%
%% Exported: http_multipart_post
%%

-spec http_multipart_post(string(), {data, binary()} | binary()) -> term().

http_multipart_post(Url, {data, Data}) ->
    FilePath = filename:join(["/tmp", ?i2l(erlang:unique_integer([positive])) ++ ".dat"]),
    ok = file:write_file(FilePath, Data),
    Result = http_multipart_post(Url, FilePath),
    ok = file:delete(FilePath),
    Result;
http_multipart_post(Url, FilePath) ->
    %% httpc does not support multipart/form-data
    Command =
        lists:flatten(
          io_lib:format("curl -s -X POST -F 'filename=@~s' ~s", [FilePath, Url])),
    Result = os:cmd(Command),
    json:decode(list_to_binary(Result)).

%%
%% Exported: bespoke_cookie
%%

-spec bespoke_cookie(db_user_serv:session_id()) -> string().

bespoke_cookie(SessionId) ->
    CookieValue = json:encode(#{<<"sessionId">> => SessionId}),
    EncodedCookieValue = uri_string:quote(CookieValue),
    %%io:format("~s\n", [http_uri:encode(CookieValue)]),
    lists:flatten(io_lib:format("bespoke=~s", [EncodedCookieValue])).

%%
%% Utilities
%%

handle_response({ok, {{"HTTP/1.1", 200, "OK"}, ResponseHeaders, Body}}) ->
    case lists:keyfind("content-type", 1, ResponseHeaders) of
        {_, "application/json"} ->
            {ok, json:decode(iolist_to_binary(Body))};
        _ ->
            {ok, Body}
    end;
handle_response({ok, {{"HTTP/1.1", StatusCode, ReasonPhrase}, ResponseHeaders, Body}}) ->
    {unexpected, StatusCode, ReasonPhrase, ResponseHeaders, Body};
handle_response({error, Reason}) ->
    {error, Reason}.
