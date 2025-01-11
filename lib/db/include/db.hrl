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
         author = not_set :: db_serv:author() | not_set | '_',
         created = not_set :: db_serv:seconds_since_epoch() | not_set | '_',
         reply_count = 0 :: integer() | '_',
         replies = [] :: [db_serv:post_id()] | '_',
         likers = [] :: [db_serv:user_id()] | '_',
         attachments = [] :: [{db_serv:attachment_path(),
                               db_serv:content_type()}] | '_'
        }).

-record(read_cache,
        {
         user_id :: db_serv:user_id(),
         post_ids = [] :: [db_serv:post_id()]
        }).

-record(meta,
        {
         type = basic :: basic,
         next_user_id = 0 :: integer(),
         next_post_id = 0 :: integer()
        }).

-record(user,
        {
         id :: db_serv:user_id() | '_',
         name :: db_user_serv:username() | '_',
         session_id = not_set :: db_user_serv:session_id() | not_set | '_',
         mac_address :: db_user_serv:mac_address() | '_',
         password_salt = not_set :: db_user_serv:password_salt() | not_set | '_',
         password_hash = not_set :: db_user_serv:password_hash() | not_set | '_',
         updated :: db_serv:seconds_since_epoch() | '_'
        }).

-endif.
