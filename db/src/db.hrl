-ifndef(DB_HRL).
-define(DB_HRL, true).

-record(post,
        {
         id = not_set :: db_serv:post_id() | not_set | '_',
         %% Note: Mandatory for top posts and disallowed for reply posts
         title = not_set :: db_serv:title() | not_set | '_',
         %% Note: Disallowed for top posts and mandatory for replt posts
         parent_post_id = not_set :: db_serv:post_id() | not_set | '_',
         %% Note: Disallowed for top posts and mandatory for reply posts
         top_post_id = not_set :: db_serv:post_id() | not_set,
         body :: db_serv:body() | '_',
         author :: db_serv:author() | '_',
         created = not_set :: db_serv:seconds_since_epoch() | not_set | '_',
         reply_count = 0 :: integer() | '_',
         replies = [] :: [db_serv:post_id()] | '_'
        }).

-record(meta,
        {
         type = basic :: basic,
         next_user_id = 0 :: integer(),
         next_post_id = 0 :: integer()
        }).

-record(user,
        {
         name :: db_user_serv:username() | '_',
         pwhash = not_set :: db_user_serv:pwhash() | not_set | '_',
         mac_address :: db_dnsmasq:mac_address() | '_',
         updated :: db_serv:seconds_since_epoch() | '_',
         session_id = not_set :: db_user_serv:session_id() | not_set | '_'
        }).

-endif.
