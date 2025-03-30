% -*- fill-column: 100; -*-

-ifndef(DB_HRL).
-define(DB_HRL, true).

-define(BESPOKE_RUNTIME_DIR, "/var/tmp/bespoke").
-define(BESPOKE_DB_DIR, filename:join(?BESPOKE_RUNTIME_DIR, "db")).
-define(BESPOKE_TMP_PATH, filename:join(?BESPOKE_RUNTIME_DIR, "tmp")).
-define(BESPOKE_MESSAGE_PATH, filename:join(?BESPOKE_RUNTIME_DIR, "message")).
-define(BESPOKE_POST_PATH, filename:join(?BESPOKE_RUNTIME_DIR, "post")).
-define(BESPOKE_FILE_PATH, filename:join(?BESPOKE_RUNTIME_DIR, "file")).

%% System disk layout:
%% ?BESPOKE_RUNTIME_DIR/db/
%%                     /tmp/

-record(meta,
        {
         type = basic :: basic,
         host = not_set :: db:host() | not_set,
         next_user_id = 0 :: db:user_id(),
         next_message_id = 0 :: db:message_id(),
         next_attachment_id = 0 :: db:attachment_id(),
         next_post_id = 0 :: integer(), %% not db:post_id() by design
         next_file_id = 0 :: db:file_id()
        }).

-record(user,
        {
         id :: db:user_id() | '_',
         name :: db:username() | '_',
         session_id = not_set :: db_user_serv:session_id() | not_set | '_',
         mac_address :: db_user_serv:mac_address() | '_',
         password_salt = not_set :: db_user_serv:password_salt() | not_set | '_',
         password_hash = not_set :: db_user_serv:password_hash() | not_set | '_',
         updated :: db:seconds_since_epoch() | '_',
         messages = [] :: [db:message_id()] | '_'
        }).

%% Direct messaging disk layout:
%% ?BESPOKE_MESSAGE_PATH/<message_id>/<user_id>
%%                                   /<user_id>-<attachment_id>
%%                                   /<user_id>-<attachment_id>.dat

-record(message,
        {
         id = not_set :: db:message_id() | not_set,
         %% Note: Disallowed for top messages and mandatory for reply messages
         top_message_id = not_set :: db:message_id() | not_set,
         author = not_set :: db:user_id() | not_set,
         created = not_set :: db:seconds_since_epoch() | not_set
        }).

%% Forum disk layout:
%% ?BESPOKE_POST_PATH/<post_id>/<attachment_id>-<filename>

-record(post,
        {
         id = not_set :: db:post_id() | not_set | '_',
         %% Note: Disallowed for reply posts and mandatory for top posts
         title = not_set :: db:title() | not_set | '_',
         %% Note: Disallowed for top posts and mandatory for reply posts
         parent_post_id = not_set :: db:post_id() | not_set | '_',
         %% Note: Disallowed for top posts and mandatory for reply posts
         top_post_id = not_set :: db:post_id() | not_set,
         body :: db:body() | '_',
         author = not_set :: db:user_id() | not_set | '_',
         created = not_set :: db:seconds_since_epoch() | not_set | '_',
         reply_count = 0 :: integer() | '_',
         replies = [] :: [db:post_id()] | '_',
         likers = [] :: [db:user_id()] | '_',
         attachments = [] :: [{main:filename(), db:content_type()}] | '_'
        }).

%% File sharing disk layout:
%% ?BESPOKE_FILE_PATH/<file_id>-<filename>

-record(file,
        {
         id = not_set :: db:file_id() | not_set,
         filename :: main:filename(),
         size :: db:file_size(),
         uploaded_size = 0 :: db:file_size(),
         content_type :: db:content_type(),
         uploader = not_set :: db:user_id() | not_set,
         created = not_set :: db:seconds_since_epoch() | not_set,
         is_uploading = false :: boolean()
        }).

-endif.
