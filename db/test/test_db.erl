-module(test_db).
-export([start/0]).

-include("../src/db.hrl").
-include("../../apptools/include/log.hrl").

start() ->
    %% No topics inserted yet
    [] = db_serv:list_topics(),
    %% Add topic
    Topic1 = #topic{title = "title1",
                    body = "body1",
                    author = "author1"},
    InsertedTopic1 = db_serv:insert_topic(Topic1),
    %% Verify topic
    [#topic{id = 0,
            title = "title1",
            body = "body1",
            author = "author1",
            created = Created,
            replies = []}] =
        db_serv:lookup_topic(InsertedTopic1#topic.id),
    is_integer(Created) == true,
    %% Add reply to topic
    Reply1 = #reply{
                topic_id = InsertedTopic1#topic.id,
                body = "reply1",
                author = "author2"},
    {ok, InsertedReply1} = db_serv:insert_reply(Reply1),
    %% Add reply to reply
    Reply2 = #reply{
                topic_id = InsertedTopic1#topic.id,
                reply_id = Reply1#reply.topic_id,
                body = "reply2",
                author = "author3"},
    {ok, InsertedReply2} = db_serv:insert_reply(Reply2),
    %% Verify topic again
    [#topic{id = 0,
            title = "title1",
            body = "body1",
            author = "author1",
            created = Created,
            replies = [0, 1]}] =
        db_serv:lookup_topic(InsertedTopic1#topic.id),
    %% Verify replies
    [InsertedReply1] = db_serv:lookup_reply(InsertedReply1#reply.id),
    [InsertedReply2] = db_serv:lookup_reply(InsertedReply2#reply.id),
    %% Stop server
    ok = db_serv:stop().
