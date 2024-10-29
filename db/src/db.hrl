-ifndef(DB_HRL).
-define(DB_HRL, true).

-record(message,
        {
         id = not_set :: db_serv:message_id() | not_set,
         %% Note: Mandatory for root messages and disallowed for other messages
         title = not_set :: db_serv:title() | not_set,
         %% Note: Disallowed for root messages and mandatory for other messages
         reply_message_id = not_set :: db_serv:message_id() | not_set,
         %% Note: Disallowed for root messages and mandatory for other messages
         root_message_id = not_set :: db_serv:message_id() | not_set,
         body :: db_serv:body(),
         author :: db_serv:author(),
         created = not_set :: db_serv:seconds_from_epoch() | not_set,
         replies = [] :: [db_serv:message_id()]
        }).

-endif.
