-ifndef(DB_HRL).
-define(DB_HRL, true).

-record(topic, {
                id = not_set :: db_serv:topic_id() | not_set,
                title :: db_serv:title(),
                body :: db_serv:body(),
                author :: db_serv:author(),
                created = not_set :: db_serv:seconds_from_epoch() | not_set,
                replies = [] :: [db_serv:reply_id()]
               }).

-record(reply, {
                id = not_set :: db_serv:reply_id() | not_set,
                topic_id :: db_serv:topic_id(),
                reply_id = not_set :: db_serv:reply_id() | not_set,
                body :: db_serv:body(),
                author :: db_serv:author(),
                created = not_set :: db_serv:seconds_from_epoch() | not_set
               }).

-endif.
