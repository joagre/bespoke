% -*- fill-column: 100; -*-

-module(webapp_cache).
-export([open/0, sync/0, mark_messages/2, marked_messages/1, mark_posts/2, marked_posts/1,
         add_challenge/2, get_challenge/1, close/0]).

-include_lib("db/include/db.hrl").

%% Message Read Cache (user-id -> [message-id, ...])
%% Description: To quickly find all messages read by a user
-define(MESSAGE_READ_FILE_PATH, filename:join(?BESPOKE_DB_DIR, "message_read.cache")).
-define(MESSAGE_READ_CACHE, message_read_cache).

%% Post Read Cache (user-id -> [post-id, ...])
%% Description: To quickly find all posts read by a user
-define(POST_READ_FILE_PATH, filename:join(?BESPOKE_DB_DIR, "post_read.cache")).
-define(POST_READ_CACHE, post_read_cache).

%% Challenge Cache
-define(CHALLENGE_CACHE, challenge_cache_GGGGG).
-define(CHALLENGE_TIMEOUT, 5 * 60). % 5 minutes in seconds
-record(challenge, {
                    username :: db_serv:username(),
                    challenge :: webapp_crypto:challenge(),
                    timestamp :: db:seconds_since_epoch()
                   }).

%%
%% Exported: open
%%

-spec open() -> ok | {error, term()}.

open() ->
    maybe
        {ok, _} ?= db:open_disk_index(?MESSAGE_READ_CACHE, ?MESSAGE_READ_FILE_PATH),
        {ok, _} ?= db:open_disk_index(?POST_READ_CACHE, ?POST_READ_FILE_PATH),
        db:open_ram(?CHALLENGE_CACHE, #challenge.username)
    end.

%%
%% Exported: sync
%%

-spec sync() -> ok | {error, term()}.

sync() ->
    maybe
        ok ?= db:sync_disk_index(?MESSAGE_READ_CACHE),
        ok ?= db:sync_disk_index(?POST_READ_CACHE)
    end.

%%
%% Exported: mark_messages
%%

-spec mark_messages(db_serv:user_id(), [db_serv:message_id()]) -> ok.

mark_messages(UserId, MessageIds) ->
    lists:foreach(fun(MessageId) ->
                          ok = db:insert_disk_index(?MESSAGE_READ_CACHE, UserId, MessageId)
                  end, MessageIds).

%%
%% Exported: marked_messages
%%

-spec marked_messages(db_serv:user_id()) -> [db_serv:message_id()] | {error, term()}.

marked_messages(UserId) ->
    db:lookup_disk_index(?MESSAGE_READ_CACHE, UserId).

%%
%% Exported: mark_posts
%%

-spec mark_posts(db_serv:user_id(), [db_serv:post_id()]) -> ok.

mark_posts(UserId, PostIds) ->
    lists:foreach(fun(PostId) ->
                          ok = db:insert_disk_index(?POST_READ_CACHE, UserId, PostId)
                  end, PostIds).

%%
%% Exported: marked_posts
%%

-spec marked_posts(db_serv:user_id()) -> [db_serv:post_id()] | {error, term()}.

marked_posts(UserId) ->
    db:lookup_disk_index(?POST_READ_CACHE, UserId).

%%
%% Exported: add_challenge
%%

-spec add_challenge(db_serv:username(), webapp_crypto:challenge()) -> ok.

add_challenge(Username, Challenge) ->
    db:insert_ram(?CHALLENGE_CACHE, #challenge{username = Username,
                                               challenge = Challenge,
                                               timestamp = db:seconds_since_epoch()}).

%%
%% Exported: get_challenge
%%

-spec get_challenge(db_serv:user_id()) -> {ok, webapp_crypto:challenge()} | {error, not_found}.

get_challenge(Username) ->
    ok = purge_challenges(),
    case db:lookup_ram(?CHALLENGE_CACHE, Username) of
        [] ->
            {error, not_found};
        [#challenge{challenge = Challenge}] ->
            {ok, Challenge}
    end.

purge_challenges() ->
    Threshold = db:seconds_since_epoch() - ?CHALLENGE_TIMEOUT,
    db:foldl_ram(?CHALLENGE_CACHE,
                 fun(#challenge{username = Username, timestamp = Timestamp}, ok)
                       when Timestamp < Threshold ->
                         db:delete_ram(?CHALLENGE_CACHE, Username);
                    (_, ok) ->
                         ok
                 end, ok).

%%
%% Exported: close
%%

-spec close() -> ok | {error, term()}.

close() ->
    maybe
        ok ?= db:close_disk_index(?MESSAGE_READ_CACHE),
        ok ?= db:close_disk_index(?POST_READ_CACHE),
        ok ?= db:close_ram_db(?CHALLENGE_CACHE)
    end.
