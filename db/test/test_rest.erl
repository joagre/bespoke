-module(test_rest).
-export([start/0]).

-include("../src/db.hrl").
-include("../../apptools/include/log.hrl").
-include("../../apptools/include/shorthand.hrl").

start() ->
    {ok, _Pid} = init_httpc(),
    %% No root messages inserted yet
    {ok, []} = http_get("http://localhost:8080/list_root_messages"),
    %% Add root message
    {ok, RootMessage1} =
        http_post("http://localhost:8080/insert_message",
                  #{<<"title">> => <<"title1">>,
                    <<"body">> => <<"body1">>,
                    <<"author">> => <<"author1">>}),
    %% Verify message
    {ok, [#{<<"id">> := 0,
            <<"title">> := <<"title1">>,
            <<"body">> := <<"body1">>,
            <<"author">> := <<"author1">>,
            <<"created">> := Created,
            <<"replies">> := []}]} =
        http_post("http://localhost:8080/lookup_messages", [0]),
    true = is_integer(Created),
    %% Add reply message to root message
    RootMessageId1 = maps:get(<<"id">>, RootMessage1),
    {ok, ReplyMessage1} =
        http_post("http://localhost:8080/insert_message",
                  #{<<"reply-message-id">> => RootMessageId1,
                    <<"root-message-id">> => RootMessageId1,
                    <<"body">> => <<"reply1">>,
                    <<"author">> => <<"author2">>}),
    %% Add repply message to reply message
    {ok, ReplyMessage2} =
        http_post("http://localhost:8080/insert_message",
                  #{<<"reply-message-id">> => maps:get(<<"id">>, ReplyMessage1),
                    <<"root-message-id">> => RootMessageId1,
                    <<"body">> => <<"reply2">>,
                    <<"author">> => <<"author3">>}),
    %% Verify root message
    ReplyMessageId1 = maps:get(<<"id">>, ReplyMessage1),
    {ok, [#{<<"replies">> := [ReplyMessageId1]}]} =
        http_post("http://localhost:8080/lookup_messages", [RootMessageId1]),
    %% Verify reply messages
    ReplyMessageId2 = maps:get(<<"id">>, ReplyMessage2),
    {ok, [#{<<"replies">> := [ReplyMessageId2]}]} =
        http_post("http://localhost:8080/lookup_messages", [ReplyMessageId1]),
    {ok, [#{<<"replies">> := []}]} =
        http_post("http://localhost:8080/lookup_messages", [ReplyMessageId2]).

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
