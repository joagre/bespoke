-ifndef(DB_MESSAGE_DB_HRL).
-define(DB_MESSAGE_DB_HRL, true).

-include("../include/db.hrl").

%% Message DB
-define(MESSAGE_FILENAME, filename:join(?BESPOKE_DB_DIR, "message.db")).
-define(MESSAGE_DB, message_db).

%% Message index DB (parent_message_id -> [child_message_id, ...])
%% Secondary index: To quickly find all direct replies to a given message
-define(MESSAGE_INDEX_FILENAME,
        filename:join(?BESPOKE_DB_DIR, "message_index.db")).
-define(MESSAGE_INDEX_DB, message_index_db).

%% Message recipient DB
-define(MESSAGE_RECIPIENT_FILENAME,
        filename:join(?BESPOKE_DB_DIR, "message_recipient.db")).
-define(MESSAGE_RECIPIENT_DB, message_recipient_db).

%% Message recipient index DB (user_id -> [message_id, message_id, ...])
%% Secondary index: To quickly find all messages for which a user is a recipient
-define(MESSAGE_RECIPIENT_INDEX_FILENAME,
        filename:join(?BESPOKE_DB_DIR, "message_recipient_index.db")).
-define(MESSAGE_RECIPIENT_INDEX_DB, message_recipient_index_db).

%% Message attachment DB
-define(MESSAGE_ATTACHMENT_FILENAME,
        filename:join(?BESPOKE_DB_DIR, "message_attachment.db")).
-define(MESSAGE_ATTACHMENT_DB, message_attachment_db).

%% Message attachment index DB (message_id -> [message_attachment_id, ...])
%% Secondary index: To quickly find all attachments for each message
-define(MESSAGE_ATTACHMENT_INDEX_FILENAME,
        filename:join(?BESPOKE_DB_DIR, "message_attachment_index.db")).
-define(MESSAGE_ATTACHMENT_INDEX_DB, message_attachment_index_db).

-endif.
