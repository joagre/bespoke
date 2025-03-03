% -*- fill-column: 100; -*-

-module(db_serv).
-export([start_link/0, stop/0,
         %% User management
         get_user_id/0,
         %% Direct messaging
         create_message/3, read_top_messages/1, read_reply_messages/2, delete_message/2,
         %% Forum
         create_post/1, read_top_posts/0, read_posts/1, read_posts/2, read_post_ids/1,
         read_post_ids/2, delete_post/1, toggle_post_like/2,
         %% File sharing
         create_file/1, read_files/0, read_files/1, delete_file/1, file_is_uploaded/1,
         %% Subscription management
         subscribe_on_changes/1,
         %% Database management
         sync/0]).
-export([message_handler/1]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("apptools/include/serv.hrl").
-include("../include/db.hrl").

-record(state, {parent :: pid()}).

%%
%% Exported: start_link
%%

-spec start_link() -> serv:spawn_server_result().

start_link() ->
    ?spawn_server(fun init/1, fun ?MODULE:message_handler/1, #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    serv:call(?MODULE, stop).

%%
%% Exported: get_user_id
%%

-spec get_user_id() -> db:user_id().

get_user_id() ->
    serv:call(?MODULE, get_user_id).

%%
%% Exported: create_message
%%

-spec create_message(#message{},
                     [{db:user_id(), main:filename()}],
                     [[{db:user_id(), main:filename()}]]) ->
          {ok, #message{}} | {error, file:posix() | access_denied}.

create_message(Message, MessageBodyBlobs, MessageAttachmentBlobs) ->
    serv:call(?MODULE, {create_message, Message, MessageBodyBlobs, MessageAttachmentBlobs}).

%%
%% Exported: read_top_messages
%%

-spec read_top_messages(db:user_id()) -> {ok, [{{#message{}, [db:attachment_id()]}}]}.

read_top_messages(UserId) ->
    serv:call(?MODULE, {read_top_messages, UserId}).

%%
%% Exported: read_reply_messages
%%

-spec read_reply_messages(db:user_id(), db:message_id()) ->
          {ok, [{{#message{}, [db:attachment_id()]}}]} |
          {error, access_denied}.

read_reply_messages(UserId, TopLevelMessageId) ->
    serv:call(?MODULE, {read_reply_messages, UserId, TopLevelMessageId}).

%%
%% Exported: delete_message
%%

-spec delete_message(db:user_id(), db:message_id()) -> ok | {error, access_denied}.

delete_message(UserId, MessageId) ->
    serv:call(?MODULE, {delete_message, UserId, MessageId}).

%%
%% Exported: create_post
%%

-spec create_post(#post{}) -> {ok, #post{}} | {error, invalid_post}.

create_post(Post) ->
    serv:call(?MODULE, {create_post, Post}).

%%
%% Exported: read_top_posts
%%

-spec read_top_posts() -> [#post{}].

read_top_posts() ->
    serv:call(?MODULE, read_top_posts).

%%
%% Exported: read_posts
%%

-spec read_posts([db:post_id()], flat | recursive) -> [#post{}].

read_posts(PostIds) ->
    read_posts(PostIds, flat).

read_posts(PostIds, Mode) ->
    serv:call(?MODULE, {read_posts, PostIds, Mode}).

%%
%% Exported: read_post_ids
%%

-spec read_post_ids([db:post_id()], flat | recursive) -> [db:post_id()].

read_post_ids(PostIds) ->
    read_post_ids(PostIds, flat).

read_post_ids(PostIds, Mode) ->
    serv:call(?MODULE, {read_post_ids, PostIds, Mode}).

%%
%% Exported: delete_post
%%

-spec delete_post(db:post_id()) -> ok | {error, not_found}.

delete_post(PostId) ->
    serv:call(?MODULE, {delete_post, PostId}).

%%
%% Exported: toggle_post_like
%%

-spec toggle_post_like(db:post_id(), db:user_id()) -> {ok, [db:user_id()]} | {error, not_found}.

toggle_post_like(PostId, UserId) ->
    serv:call(?MODULE, {toggle_post_like, PostId, UserId}).

%%
%% Exported: create_file
%%

-spec create_file(#file{}) -> {ok, #file{}} | {error, term()}.

create_file(File) ->
    serv:call(?MODULE, {create_file, File}).

%%
%% Exported: read_files
%%

-spec read_files() -> [#file{}].

read_files() ->
    serv:call(?MODULE, read_files).

%%
%% Exported: read_files
%%

-spec read_files([db:file_id()]) -> [#file{}].

read_files(FileIds) ->
    serv:call(?MODULE, {read_files, FileIds}).

%%
%% Exported: delete_file
%%

-spec delete_file(db:file_id()) -> ok | {error, not_found}.

delete_file(FileId) ->
    serv:call(?MODULE, {delete_file, FileId}).

%%
%% file_is_uploaded
%%

-spec file_is_uploaded(db:file_id()) -> ok | {error, not_found}.

file_is_uploaded(FileId) ->
    serv:call(?MODULE, {file_is_uploaded, FileId}).

%%
%% Exported: subscribe_on_changes
%%

-spec subscribe_on_changes([db:post_id()]) -> db_subscription_db:subscription_id().

subscribe_on_changes(PostIds) ->
    serv:call(?MODULE, {subscribe_on_changes, self(), PostIds}).

%%
%% Exported: sync
%%

-spec sync() -> ok.

sync() ->
    serv:call(?MODULE, sync).

%%
%% Server
%%

init(Parent) ->
    ok = open_dbs(),
    ?log_info("Database server has been started"),
    {ok, #state{parent = Parent}}.

message_handler(S) ->
    receive
        {call, From, stop = Call} ->
            ?log_debug("Call: ~p", [Call]),
            ok = close_dbs(),
            {reply, From, ok};
        %% User management
        {call, From, get_user_id = Call} ->
            ?log_debug("Call: ~p", [Call]),
            NextUserId = db_meta_db:read_next_user_id(),
            {reply, From, NextUserId};
        %% Direct messaging
        {call, From, {create_message, Message, BodyBlobs, AttachmentBlobs} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, db_message_db:create_message(Message, BodyBlobs, AttachmentBlobs)};
        {call, From, {read_top_messages, UserId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, db_message_db:read_top_messages(UserId)};
        {call, From, {read_reply_messages, UserId, TopMessageId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, db_message_db:read_reply_messages(UserId, TopMessageId)};
        {call, From, {delete_message, UserId, MessageId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, db_message_db:delete_message(UserId, MessageId)};
        %% Forum
        {call, From, {create_post, Post} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, db_post_db:create_post(Post)};
        {call, From, read_top_posts = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, db_post_db:read_top_posts()};
        {call, From, {read_posts, PostIds, Mode} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, db_post_db:read_posts(PostIds, Mode)};
        {call, From, {read_post_ids, PostIds, Mode} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, read_post_ids(PostIds, Mode)};
        {call, From, {delete_post, PostId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, db_post_db:delete_post(PostId)};
        {call, From, {toggle_post_like, PostId, UserId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, db_post_db:toggle_post_like(PostId, UserId)};
        %% File sharing
        {call, From, {create_file, File} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, db_file_db:create_file(File)};
        {call, From, read_files = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, db_file_db:read_files()};
        {call, From, {read_files, FileIds} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, db_file_db:read_files(FileIds)};
        {call, From, {delete_file, FileId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, db_file_db:delete_file(FileId)};
        {call, From, {file_is_uploaded, FileId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, db_file_db:file_is_uploaded(FileId)};
        %% Subscription management
        {call, From, {subscribe_on_changes, Subscriber, PostIds} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, db_subscription_db:subscribe(Subscriber, PostIds)};
        {'DOWN', MonitorRef, process, Pid, _Reason} ->
            ?log_info("Subscriber died: ~w", [Pid]),
            ok = db_subscription_db:unsubscribe(MonitorRef),
            noreply;
        %% Database management
        {call, From, sync = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, sync_dbs()};
        %% System messages
        {'EXIT', Pid, Reason} when S#state.parent == Pid ->
            ok = close_dbs(),
            exit(Reason);
        {system, From, Request} ->
            ?log_debug("System: ~p", [Request]),
            {system, From, Request};
        UnknownMessage ->
            ?log_error("Unknown message: ~p", [UnknownMessage]),
            noreply
    end.

open_dbs() ->
    ok = db_meta_db:open(),
    ok = db_message_db:open(),
    ok = db_post_db:open(),
    ok = db_file_db:open(),
    ok = db_subscription_db:open().

close_dbs() ->
    _ = db_subscription_db:close(),
    _ = db_file_db:close(),
    _ = db_post_db:close(),
    _ = db_message_db:close(),
    _ = db_meta_db:close(),
    ok.

sync_dbs() ->
    ok = db_file_db:sync(),
    ok = db_post_db:sync(),
    ok = db_message_db:sync(),
    db_meta_db:sync().
