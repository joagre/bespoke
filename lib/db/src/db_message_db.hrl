-ifndef(DB_MESSAGE_DB_HRL).
-define(DB_MESSAGE_DB_HRL, true).

-include("../include/db.hrl").

%% Message DB
-define(MESSAGE_FILE_PATH, filename:join(?BESPOKE_DB_DIR, "message.db")).
-define(MESSAGE_DB, message_db).

%% Top Message Index DB (user-id -> [top-message-id, ...])
%% Description: To quickly find all top messages for a user
-define(MESSAGE_TOP_INDEX_FILE_PATH,
         filename:join(?BESPOKE_DB_DIR, "message_top_index.db")).
-define(MESSAGE_TOP_INDEX_DB, message_top_index_db).

%% Reply Index DB (top-message-id -> [message-id, ...])
%% Description: To quickly find all reply messages to a top message
-define(MESSAGE_REPLY_INDEX_FILE_PATH,
        filename:join(?BESPOKE_DB_DIR, "message_recipient_index.db")).
-define(MESSAGE_REPLY_INDEX_DB, message_reply_index_db).

%% Recipient Index DB (message-id -> [user-id, ...])
%% Description: To quickly find all recipients to a message
-define(MESSAGE_RECIPIENT_INDEX_FILE_PATH,
        filename:join(?BESPOKE_DB_DIR, "message_recipient_index.db")).
-define(MESSAGE_RECIPIENT_INDEX_DB, message_recipient_index_db).

%% Attachment Index DB (message-id -> [attachment-id, ...])
%% Description: To quickly find all attachments to a message
-define(MESSAGE_ATTACHMENT_INDEX_FILE_PATH,
        filename:join(?BESPOKE_DB_DIR, "message_attachment_index.db")).
-define(MESSAGE_ATTACHMENT_INDEX_DB, message_attachment_index_db).

-endif.
