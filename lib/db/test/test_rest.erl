-module(test_rest).
-export([start/0]).

-include("../src/db.hrl").
-include("../../apptools/include/log.hrl").
-include("../../apptools/include/shorthand.hrl").

start() ->
    {ok, _Pid} = init_httpc(),
    %% No top posts inserted yet
    {ok, []} = http_get("http://localhost/list_top_posts"),
    %% Add top post
    {ok, TopPost1} =
        http_post("http://localhost/insert_post",
                  #{<<"title">> => <<"title1">>,
                    <<"body">> => <<"body1">>,
                    <<"author">> => <<"author1">>}),
    %% Verify post
    {ok, [#{<<"id">> := <<"0">>,
            <<"title">> := <<"title1">>,
            <<"body">> := <<"body1">>,
            <<"author">> := <<"author1">>,
            <<"created">> := Created,
            <<"reply-count">> := 0,
            <<"replies">> := []}]} =
        http_post("http://localhost/lookup_posts", [<<"0">>]),
    true = is_integer(Created),
    %% Add reply post to top post
    TopPostId1 = maps:get(<<"id">>, TopPost1),
    {ok, ReplyPost1} =
        http_post("http://localhost/insert_post",
                  #{<<"parent-post-id">> => TopPostId1,
                    <<"top-post-id">> => TopPostId1,
                    <<"body">> => <<"reply1">>,
                    <<"author">> => <<"author2">>}),
    %% Add repply post to reply post
    {ok, ReplyPost2} =
        http_post("http://localhost/insert_post",
                  #{<<"parent-post-id">> => maps:get(<<"id">>, ReplyPost1),
                    <<"top-post-id">> => TopPostId1,
                    <<"body">> => unicode:characters_to_binary("öööreply2"),
                    <<"author">> => <<"author3">>}),
    %% Verify top post
    ReplyPostId1 = maps:get(<<"id">>, ReplyPost1),
    {ok, [#{<<"replies">> := [ReplyPostId1]}]} =
        http_post("http://localhost/lookup_posts", [TopPostId1]),
    %% Verify reply posts
    ReplyPostId2 = maps:get(<<"id">>, ReplyPost2),
    {ok, [#{<<"replies">> := [ReplyPostId2]}]} =
        http_post("http://localhost/lookup_posts", [ReplyPostId1]),
    {ok, [#{<<"replies">> := []}]} =
        http_post("http://localhost/lookup_posts", [ReplyPostId2]),
    %% Verify reply counts
    {ok, [#{<<"reply-count">> := 2}]} =
        http_post("http://localhost/lookup_posts", [TopPostId1]),
    {ok, [#{<<"reply-count">> := 1}]} =
        http_post("http://localhost/lookup_posts", [ReplyPostId1]),
    {ok, [#{<<"reply-count">> := 0}]} =
        http_post("http://localhost/lookup_posts", [ReplyPostId2]).

%%
%% Utilities
%%

init_httpc() ->
    application:start(inets),
    inets:start(httpc, [{profile, poop}]).

http_get(Url) ->
    case httpc:request(get, {Url, [{"connection", "close"}]},
                       [{ssl, [{verify, verify_none}]}], []) of
        {ok, {{"HTTP/1.1", 200, "OK"}, Headers, Body}} ->
            case lists:keyfind("content-type", 1, Headers) of
                {_, "application/json"} ->
                    {ok, json:decode(iolist_to_binary(Body))};
                _ ->
                    {ok, Body}
            end;
        {ok, {{"HTTP/1.1", StatusCode, ReasonPhrase}}, Headers, Body} ->
            {unexpected, StatusCode, ReasonPhrase, Headers, Body};
        {error, Reason} ->
            {error, Reason}
    end.

http_post(Url, Data) ->
    case httpc:request(post, {Url,
                              [{"connection", "close"}],
                              "application/json",
                              json:encode(Data)},
                       [{ssl, [{verify, verify_none}]}], []) of
        {ok, {{"HTTP/1.1", 200, "OK"}, Headers, Body}} ->
            case lists:keyfind("content-type", 1, Headers) of
                {_, "application/json"} ->
                    {ok, json:decode(iolist_to_binary(Body))};
                _ ->
                    {ok, Body}
            end;
        {ok, {{"HTTP/1.1", StatusCode, ReasonPhrase}}, Headers, Body} ->
            {unexpected, StatusCode, ReasonPhrase, Headers, Body};
        {error, Reason} ->
            {error, Reason}
    end.
