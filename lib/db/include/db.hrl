-ifndef(DB_HRL).
-define(DB_HRL, true).

-record(meta,
        {
         type = basic :: basic,
         host = not_set :: db_serv:host() | not_set,
         next_message_id = 0 :: db_serv:message_id(),
         next_post_id = 0 :: integer(),
         next_file_id = 0 :: db_serv:file_id(),
         next_user_id = 0 :: db_serv:user_id()
        }).

-record(user,
        {
         id :: db_serv:user_id() | '_', %% primary key
         name :: db_user_serv:username() | '_',
         session_id = not_set :: db_user_serv:session_id() | not_set | '_',
         mac_address :: db_user_serv:mac_address() | '_',
         password_salt = not_set :: db_user_serv:password_salt() | not_set | '_',
         password_hash = not_set :: db_user_serv:password_hash() | not_set | '_',
         updated :: db_serv:seconds_since_epoch() | '_',
         messages = [] :: [db_serv:message_id()] | '_'
        }).

%% message/<message_id>/
%% message/<message_id>/attachment/<attachment_id>-<user-id>

-record(message,
        {
         id = not_set :: db_serv:message_id() | not_set | '_', %% primary key
         %% Note: Mandatory for top messages and disallowed for reply messages
         title = not_set :: db_serv:title() | not_set | '_',
         %% Note: Mandatory for top messages and disallowed for reply messages
         recipients = [] :: [db_serv:user_id()] | '_',
         %% Note: Mandatory for top messages and disallowed for reply messages
         replies = [] :: [db_serv:message_id()] | '_',
         %% Note: Disallowed for top messages and mandatory for reply messages
         parent_message_id = not_set :: db_serv:message_id() | not_set | '_',
         %% COMMENT: Switch to user_id everyehere and use username during the rest comunication.
         author = not_set :: db_serv:user_id() | not_set | '_',
         created = not_set :: db_serv:seconds_since_epoch() | not_set | '_',
         attachments = [] :: [db_serv:attachment_id()] | '_'
        }).

-record(post,
        {
         id = not_set :: db_serv:post_id() | not_set | '_',
         %% Note: Mandatory for top posts and disallowed for reply posts
         title = not_set :: db_serv:title() | not_set | '_',
         %% Note: Disallowed for top posts and mandatory for reply posts
         parent_post_id = not_set :: db_serv:post_id() | not_set | '_',
         %% Note: Disallowed for top posts and mandatory for reply posts
         top_post_id = not_set :: db_serv:post_id() | not_set,
         body :: db_serv:body() | '_',
         author = not_set :: db_serv:user_id() | not_set | '_',
         created = not_set :: db_serv:seconds_since_epoch() | not_set | '_',
         reply_count = 0 :: integer() | '_',
         replies = [] :: [db_serv:post_id()] | '_',
         likers = [] :: [db_serv:user_id()] | '_',
         attachments = [] :: [{db_serv:attachment_path(),
                               db_serv:content_type()}] | '_'
        }).

-record(file,
        {
         id = not_set :: db_serv:file_id() | not_set,
         filename :: db_serv:filename(),
         size :: db_serv:file_size(),
         uploaded_size = 0 :: db_serv:file_size(),
         content_type :: db_serv:content_type(),
         uploader = not_set :: db_serv:user_id() | not_set,
         created = not_set :: db_serv:seconds_since_epoch() | not_set,
         is_uploading = false :: boolean()
        }).

-record(read_cache,
        {
         user_id :: db_serv:user_id(),
         post_ids = [] :: [db_serv:post_id()]
        }).

-endif.
