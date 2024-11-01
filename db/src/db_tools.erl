-module(db_tools).
-export([create_dummy_db/0]).

-include("db.hrl").
-include("../../apptools/include/log.hrl").

create_dummy_db() ->
    %% Add two root messages
    RootMessage1 = #message{title = "Var har sillen tagit vägen?",
                            body = "body",
                            author = "ginko4711"},
    {ok, InsertedRootMessage1} = db_serv:insert_message(RootMessage1),
    RootMessage2 = #message{title = "Republik nu!",
                            body = "body1",
                            author = "zappeU"},
    {ok, _InsertedRootMessage2} = db_serv:insert_message(RootMessage2),
    %% Add two replies
    ReplyMessage1 =
        #message{reply_message_id = InsertedRootMessage1#message.id,
                 root_message_id = InsertedRootMessage1#message.id,
                 body = "reply1",
                 author = "harald"},
    {ok, InsertedReplyMessage1} = db_serv:insert_message(ReplyMessage1),
    ReplyMessage2 =
        #message{reply_message_id = InsertedRootMessage1#message.id,
                 root_message_id = InsertedRootMessage1#message.id,
                 body = "reply1",
                 author = "harald"},
    {ok, _InsertedReplyMessage2} = db_serv:insert_message(ReplyMessage2),
    %% Add two replies to the first reply
    ReplyMessage11 =
        #message{reply_message_id = InsertedReplyMessage1#message.id,
                 root_message_id = InsertedRootMessage1#message.id,
                 body = "reply2",
                 author = "sminkor"},
    {ok, _InsertedReplyMessage11} = db_serv:insert_message(ReplyMessage11),
    ReplyMessage12 =
        #message{reply_message_id = InsertedReplyMessage1#message.id,
                 root_message_id = InsertedRootMessage1#message.id,
                 body = "reply2",
                 author = "hönan"},
    {ok, _InsertedReplyMessage12} = db_serv:insert_message(ReplyMessage12),
    ok = db_serv:sync().
