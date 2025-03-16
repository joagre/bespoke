% -*- fill-column: 100; -*-

-module(webapp_cache).
-export([open/0, sync/0]).
%% Message read cache
-export([mark_messages_as_read/2, list_read_messages/1]).
%% Post read cache
-export([mark_posts_as_read/2, list_read_posts/1]).
%% Challenge cache
-export([add_challenge/2, get_challenge/1, close/0]).

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
-define(CHALLENGE_CACHE, challenge_cache).
-define(CHALLENGE_TIMEOUT, 5 * 60). % 5 minutes in seconds
-record(challenge, {
                    username :: db:username(),
                    challenge :: webapp_crypto:challenge(),
                    timestamp :: db:seconds_since_epoch()
                   }).

%%
%% Exported: open
%%

-spec open() -> ok | {error, term()}.

open() ->
    maybe
        {ok, _} ?= idets:open_file(?MESSAGE_READ_CACHE, ?MESSAGE_READ_FILE_PATH),
        {ok, _} ?= idets:open_file(?POST_READ_CACHE, ?POST_READ_FILE_PATH),
        ?CHALLENGE_CACHE =
            ets:new(?CHALLENGE_CACHE, [{keypos, #challenge.username}, named_table, public]),
        ok
    end.

%%
%% Exported: sync
%%

-spec sync() -> ok | {error, term()}.

sync() ->
    maybe
        ok ?= idets:sync(?MESSAGE_READ_CACHE),
        ok ?= idets:sync(?POST_READ_CACHE)
    end.

%%
%% Exported: mark_messages_as_read
%%

-spec mark_messages_as_read(db:user_id(), [db:message_id()]) -> ok.

mark_messages_as_read(UserId, MessageIds) ->
    lists:foreach(fun(MessageId) ->
                          ok = idets:insert(?MESSAGE_READ_CACHE, UserId, MessageId)
                  end, MessageIds).

%%
%% Exported: list_read_messages
%%

-spec list_read_messages(db:user_id()) -> [db:message_id()] | {error, term()}.

list_read_messages(UserId) ->
    idets:lookup(?MESSAGE_READ_CACHE, UserId).

%%
%% Exported: mark_posts_as_read
%%

-spec mark_posts_as_read(db:user_id(), [db:post_id()]) -> ok.

mark_posts_as_read(UserId, PostIds) ->
    lists:foreach(fun(PostId) ->
                          ok = idets:insert(?POST_READ_CACHE, UserId, PostId)
                  end, PostIds).

%%
%% Exported: list_read_posts
%%

-spec list_read_posts(db:user_id()) -> [db:post_id()] | {error, term()}.

list_read_posts(UserId) ->
    idets:lookup(?POST_READ_CACHE, UserId).

%%
%% Exported: add_challenge
%%

-spec add_challenge(db:username(), webapp_crypto:challenge()) -> ok.

add_challenge(Username, Challenge) ->
    true = ets:insert(?CHALLENGE_CACHE, #challenge{username = Username,
                                                   challenge = Challenge,
                                                   timestamp = db:seconds_since_epoch()}),
    ok.

%%
%% Exported: get_challenge
%%

-spec get_challenge(db:username()) -> {ok, webapp_crypto:challenge()} | {error, not_found}.

get_challenge(Username) ->
    ok = purge_challenges(),
    case ets:lookup(?CHALLENGE_CACHE, Username) of
        [] ->
            {error, not_found};
        [#challenge{challenge = Challenge}] ->
            {ok, Challenge}
    end.

purge_challenges() ->
    Threshold = db:seconds_since_epoch() - ?CHALLENGE_TIMEOUT,
    ets:foldl(fun(#challenge{username = Username, timestamp = Timestamp}, ok)
                    when Timestamp < Threshold ->
                      true = ets:delete(?CHALLENGE_CACHE, Username),
                      ok;
                 (_, ok) ->
                      ok
              end, ok, ?CHALLENGE_CACHE).

%%
%% Exported: close
%%

-spec close() -> ok | {error, term()}.

close() ->
    maybe
        ok ?= idets:close(?MESSAGE_READ_CACHE),
        ok ?= idets:close(?POST_READ_CACHE),
        true ?= ets:delete(?CHALLENGE_CACHE),
        ok
    end.
