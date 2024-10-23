-module(test_rest).
-export([start/0]).

-include("../src/db.hrl").
-include("../../apptools/include/log.hrl").
-include("../../apptools/include/shorthand.hrl").

start() ->
    {ok, _Pid} = init_httpc(),
    %% No topics inserted yet
    {ok, []} = http_get("http://localhost:8080/list_topics"),
    %% Add topic
    {ok, InsertedTopic1} =
        http_post("http://localhost:8080/insert_topic",
                  #{<<"title">> => <<"title1">>,
                    <<"body">> => <<"body1">>,
                    <<"author">> => <<"author1">>}),
    %% Verify topic
    {ok, #{<<"id">> := 0,
           <<"title">> := <<"title1">>,
           <<"body">> := <<"body1">>,
           <<"author">> := <<"author1">>,
           <<"created">> := Created,
           <<"replies">> := []}} =
        http_get("http://localhost:8080/lookup_topic/" ++
                     ?i2l(maps:get(<<"id">>, InsertedTopic1))),
    true = is_integer(Created),
    %% Add reply to topic
    {ok, InsertedReply1} =
        http_post("http://localhost:8080/insert_reply",
                  #{<<"topic-id">> => maps:get(<<"id">>, InsertedTopic1),
                    <<"body">> => <<"reply1">>,
                    <<"author">> => <<"author2">>}),
    %% Add reply to reply
    {ok, InsertedReply2} =
        http_post("http://localhost:8080/insert_reply",
                  #{<<"topic-id">> => maps:get(<<"id">>, InsertedTopic1),
                    <<"reply-id">> => maps:get(<<"id">>, InsertedReply1),
                    <<"body">> => <<"reply2">>,
                    <<"author">> => <<"author3">>}),
    %% Verify topic again
    {ok, #{<<"id">> := 0,
           <<"title">> := <<"title1">>,
           <<"body">> := <<"body1">>,
           <<"author">> := <<"author1">>,
           <<"created">> := Created,
           <<"replies">> := [0, 1]}} =
        http_get("http://localhost:8080/lookup_topic/" ++
                     ?i2l(maps:get(<<"id">>, InsertedTopic1))),
    %% Verify replies
    {ok, InsertedReply1} =
        http_get("http://localhost:8080/lookup_reply/" ++
                     ?i2l(maps:get(<<"id">>, InsertedReply1))),
    {ok, InsertedReply2} =
        http_get("http://localhost:8080/lookup_reply/" ++
                     ?i2l(maps:get(<<"id">>, InsertedReply2))).

%%
%% Utilities
%%

init_httpc() ->
    application:start(inets),
    inets:start(httpc, [{profile, poop}]).

http_get(Url) ->
    case httpc:request(Url) of
        {ok, {{"HTTP/1.1", 200, "OK"}, Headers, Body}} ->
            case lists:keyfind("content-type", 1, Headers) of
                {_, "application/json"} ->
                    {ok, jsone:decode(iolist_to_binary(Body))};
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
                       [], []) of
        {ok, {{"HTTP/1.1", 200, "OK"}, Headers, Body}} ->
            case lists:keyfind("content-type", 1, Headers) of
                {_, "application/json"} ->
                    {ok, jsone:decode(iolist_to_binary(Body))};
                _ ->
                    {ok, Body}
            end;
        {ok, {{"HTTP/1.1", StatusCode, ReasonPhrase}}, Headers, Body} ->
            {unexpected, StatusCode, ReasonPhrase, Headers, Body};
        {error, Reason} ->
            {error, Reason}
    end.
