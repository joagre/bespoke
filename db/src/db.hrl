-ifndef(DB_HRL).
-define(DB_HRL, true).

-record(message,
        {
         id = not_set :: db_serv:message_id() | not_set | '_',
         %% Note: Mandatory for root messages and disallowed for other messages
         title = not_set :: db_serv:title() | not_set | '_',
         %% Note: Disallowed for root messages and mandatory for other messages
         parent_message_id = not_set :: db_serv:message_id() | not_set | '_',
         %% Note: Disallowed for root messages and mandatory for other messages
         root_message_id = not_set :: db_serv:message_id() | not_set,
         body :: db_serv:body() | '_',
         author :: db_serv:author() | '_',
         created = not_set :: db_serv:seconds_since_epoch() | not_set | '_',
         reply_count = 0 :: integer() | '_',
         replies = [] :: [db_serv:message_id()] | '_'
        }).

-record(meta,
        {
         type = basic :: basic,
         next_message_id = 0 :: integer()
        }).

-record(alias,
        {
         name :: binary() | '_',
         pwhash = not_set :: binary() | not_set | '_',
         session_id = not_set :: binary() | not_set | '_',
         mac_address :: db_dnsmasq:mac_address()
        }).

-endif.
