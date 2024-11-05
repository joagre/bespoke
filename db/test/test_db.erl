-module(test_db).
-export([start/0]).

-include("../src/db.hrl").
-include("../../apptools/include/log.hrl").

start() ->
    %% No root messages inserted yet
    [] = db_serv:list_root_messages(),
    %% Add root message
    RootMessage1 = #message{title = "title1",
                            body = "body1",
                            author = "author1"},
    {ok, InsertedRootMessage1} = db_serv:insert_message(RootMessage1),
    %% Verify message
    [#message{id = <<"0">>,
              title = "title1",
              parent_message_id = not_set,
              root_message_id = not_set,
              body = "body1",
              author = "author1",
              created = Created,
              reply_count = 0,
              replies = []}] =
        db_serv:lookup_messages([InsertedRootMessage1#message.id]),
    true = is_integer(Created),
    %% Add reply message to root message
    ReplyMessage1 =
        #message{parent_message_id = InsertedRootMessage1#message.id,
                 root_message_id = InsertedRootMessage1#message.id,
                 body = "reply1",
                 author = "author2"},
    {ok, InsertedReplyMessage1} = db_serv:insert_message(ReplyMessage1),
    %% Add reply message to reply message
    ReplyMessage2 =
        #message{parent_message_id = InsertedReplyMessage1#message.id,
                 root_message_id = InsertedRootMessage1#message.id,
                 body = "reply2",
                 author = "author3"},
    {ok, InsertedReplyMessage2} = db_serv:insert_message(ReplyMessage2),
    %% Verify root message
    ReplyMessageId1 = InsertedReplyMessage1#message.id,
    [#message{replies = [ReplyMessageId1]}] =
        db_serv:lookup_messages([InsertedRootMessage1#message.id]),
    %% Verify reply messages
    ReplyMessageId2 = InsertedReplyMessage2#message.id,
    [#message{replies = [ReplyMessageId2]}] =
        db_serv:lookup_messages([InsertedReplyMessage1#message.id]),
    [#message{replies = []}] =
        db_serv:lookup_messages([InsertedReplyMessage2#message.id]),
    %% Verify reply counts
    [#message{reply_count = 2}] =
        db_serv:lookup_messages([InsertedRootMessage1#message.id]),
    [#message{reply_count = 1}] =
        db_serv:lookup_messages([InsertedReplyMessage1#message.id]),
    [#message{reply_count = 0}] =
        db_serv:lookup_messages([InsertedReplyMessage2#message.id]).
