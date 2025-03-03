% -*- fill-column: 100; -*-

-module(db_serv).
-export([start_link/0, stop/0]).
-export([get_user_id/0]).
-export([create_message/3, read_top_messages/1, read_reply_messages/2, delete_message/2]).
-export([create_post/1, read_top_posts/0, read_posts/1, read_posts/2, read_post_ids/1,
         read_post_ids/2, delete_post/1, toggle_post_like/2]).
-export([create_file/1, read_files/0, read_files/1, delete_file/1, file_is_uploaded/1]).
-export([subscribe_on_changes/1]).
-export([sync/0]).
-export([message_handler/1]).
-export_type([ssid/0, host/0, user_id/0, username/0, message_id/0, attachment_id/0, post_id/0,
              title/0, body/0, seconds_since_epoch/0, content_type/0, file_id/0, file_size/0,
              subscription_id/0, monitor_ref/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("apptools/include/serv.hrl").
-include("../include/db.hrl").

%% Post DB
-define(POST_FILE_PATH, filename:join(?BESPOKE_DB_DIR, "post.db")).
-define(POST_DB, post_db).

%% File DB
-define(FILE_FILE_PATH, filename:join(?BESPOKE_DB_DIR, "file.db")).
-define(FILE_DB, file_db).

%% Subscription DB
-define(SUBSCRIPTION_DB, subscription_db).

%% Types
-type ssid() :: binary().
-type host() :: binary().
-type user_id() :: integer().
-type username() :: binary().
-type message_id() :: integer().
-type attachment_id() :: integer().
-type post_id() :: binary().
-type title() :: binary().
-type body() :: binary().
-type seconds_since_epoch() :: integer().
-type content_type() :: binary().
-type file_id() :: integer().
-type file_size() :: integer().
-type subscription_id() :: reference().
-type monitor_ref() :: reference().

-record(state, {parent :: pid()}).

-record(subscription, {
                       id :: subscription_id() | '_',
                       subscriber :: pid() | '_',
                       monitor_ref :: monitor_ref(),
                       post_ids :: [post_id()] | '_'
                      }).

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

-spec get_user_id() -> user_id().

get_user_id() ->
    serv:call(?MODULE, get_user_id).

%%
%% Exported: create_message
%%

-spec create_message(#message{},
                     [{db_serv:user_id(), main:filename()}],
                     [[{db_serv:user_id(), main:filename()}]]) ->
          {ok, #message{}} | {error, file:posix() | access_denied}.

create_message(Message, MessageBodyBlobs, MessageAttachmentBlobs) ->
    serv:call(?MODULE, {create_message, Message, MessageBodyBlobs, MessageAttachmentBlobs}).

%%
%% Exported: read_top_messages
%%

-spec read_top_messages(db_serv:user_id()) ->
          {ok, [{{#message{}, [db_serv:attachment_id()]}}]}.

read_top_messages(UserId) ->
    serv:call(?MODULE, {read_top_messages, UserId}).

%%
%% Exported: read_reply_messages
%%

-spec read_reply_messages(db_serv:user_id(), db_serv:message_id()) ->
          {ok, [{{#message{}, [db_serv:attachment_id()]}}]} |
          {error, access_denied}.

read_reply_messages(UserId, TopLevelMessageId) ->
    serv:call(?MODULE, {read_reply_messages, UserId, TopLevelMessageId}).

%%
%% Exported: delete_message
%%

-spec delete_message(db_serv:user_id(), db_serv:message_id()) ->
          ok | {error, access_denied}.

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

-spec read_posts([post_id()], flat | recursive) -> [#post{}].

read_posts(PostIds) ->
    read_posts(PostIds, flat).

read_posts(PostIds, Mode) ->
    serv:call(?MODULE, {read_posts, PostIds, Mode}).

%%
%% Exported: read_post_ids
%%

-spec read_post_ids([post_id()], flat | recursive) -> [post_id()].

read_post_ids(PostIds) ->
    read_post_ids(PostIds, flat).

read_post_ids(PostIds, Mode) ->
    serv:call(?MODULE, {read_post_ids, PostIds, Mode}).

%%
%% delete_post
%%

-spec delete_post(post_id()) -> ok | {error, not_found}.

delete_post(PostId) ->
    serv:call(?MODULE, {delete_post, PostId}).

%%
%% Exported: toggle_post_like
%%

-spec toggle_post_like(post_id(), user_id()) ->
          {ok, [user_id()]} | {error, not_found}.

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

-spec read_files([file_id()]) -> [#file{}].

read_files(FileIds) ->
    serv:call(?MODULE, {read_files, FileIds}).

%%
%% delete_file
%%

-spec delete_file(file_id()) -> ok | {error, not_found}.

delete_file(FileId) ->
    serv:call(?MODULE, {delete_file, FileId}).

%%
%% file_is_uploaded
%%

-spec file_is_uploaded(file_id()) -> ok | {error, not_found}.

file_is_uploaded(FileId) ->
    serv:call(?MODULE, {file_is_uploaded, FileId}).

%%
%% Exported: subscribe_on_changes
%%

-spec subscribe_on_changes([post_id()]) -> subscription_id().

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
            {reply, From, do_insert_post(Post)};
        {call, From, read_top_posts = Call} ->
            ?log_debug("Call: ~p", [Call]),
            TopPosts = dets:match_object(?POST_DB, #post{top_post_id = not_set, _ = '_'}),
            SortedTopPosts = lists:sort(fun(PostA, PostB) ->
                                                PostA#post.created > PostB#post.created
                                        end, TopPosts),
            {reply, From, SortedTopPosts};
        {call, From, {read_posts, PostIds, Mode} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, do_lookup_posts(PostIds, Mode)};
        {call, From, {read_post_ids, PostIds, Mode} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, do_lookup_post_ids(PostIds, Mode)};
        {call, From, {delete_post, PostId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:lookup(?POST_DB, PostId) of
                [#post{parent_post_id = ParentPostId}] when ParentPostId /= not_set ->
                    [ParentPost] = dets:lookup(?POST_DB, ParentPostId),
                    ok = insert_and_inform(
                           ParentPost#post{
                             replies = lists:delete(PostId, ParentPost#post.replies)}),
                    N = delete_all([PostId]),
                    ok = update_parent_count(ParentPost#post.id, -N),
                    {reply, From, ok};
                [_TopPost] ->
                    _N = delete_all([PostId]),
                    {reply, From, ok};
                [] ->
                    {reply, From, {error, not_found}}
            end;
        {call, From, {toggle_post_like, PostId, UserId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:lookup(?POST_DB, PostId) of
                [#post{likers = Likers} = Post] ->
                    UpdatedLikers = case lists:member(UserId, Likers) of
                                        true ->
                                            lists:delete(UserId, Likers);
                                        false ->
                                            [UserId|Likers]
                                    end,
                    ok = insert_and_inform(Post#post{likers = UpdatedLikers}),
                    {reply, From, {ok, UpdatedLikers}};
                [] ->
                    {reply, From, {error, not_found}}
            end;
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
        %% Subscription handling
        {call, From, {subscribe_on_changes, Subscriber, PostIds} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            SubscriptionId = make_ref(),
            true = ets:insert(?SUBSCRIPTION_DB, #subscription{
                                                   id = SubscriptionId,
                                                   subscriber = Subscriber,
                                                   monitor_ref = monitor(process, Subscriber),
                                                   post_ids = PostIds}),
            {reply, From, SubscriptionId};
        {'DOWN', MonitorRef, process, Pid, _Reason} ->
            ?log_info("Subscriber died: ~w", [Pid]),
            true = ets:match_delete(?SUBSCRIPTION_DB,
                                    #subscription{monitor_ref = MonitorRef, _ = '_'}),
            noreply;
        %% System
        {call, From, sync = Call} ->
            ?log_debug("Call: ~p", [Call]),
            ok = sync_dbs(),
            {reply, From, ok};
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

%%
%% Database utilities
%%

open_dbs() ->
    ok = db_meta_db:open(),
    ok = db_message_db:open(),
    {ok, _} = db:open_disk(?POST_DB, ?POST_FILE_PATH, #post.id),
    {ok, _} = db:open_disk(?FILE_DB, ?FILE_FILE_PATH, #file.id),
    db:open_ram(?SUBSCRIPTION_DB, #subscription.id).

close_dbs() ->
    _ = db:close_disk(?FILE_DB),
    _ = db:close_disk(?POST_DB),
    _ = db_message_db:close(),
    _ = db_meta_db:close(),
    ok.

sync_dbs() ->
    ok = db:sync_disk(?FILE_DB),
    ok = db:sync_disk(?POST_DB),
    ok = db_message_db:sync(),
    db_meta_db:sync().

%%
%% Lookup posts
%%

do_lookup_posts(PostIds, Mode) ->
    Posts =
        lists:foldr(
          fun(PostId, Acc) ->
                  case dets:lookup(?POST_DB, PostId) of
                      [Post] when Mode == flat ->
                          [Post|Acc];
                      [Post] when Mode == recursive ->
                          Replies = do_lookup_posts(Post#post.replies, Mode),
                          [Post|Acc] ++ Replies
                  end
          end, [], PostIds),
    lists:sort(
      fun(PostA, PostB) ->
              PostA#post.created =< PostB#post.created
      end, Posts).

do_lookup_post_ids(PostIds, Mode) ->
    lists:foldr(
      fun(PostId, Acc) ->
              case dets:lookup(?POST_DB, PostId) of
                  [Post] when Mode == flat ->
                      [Post#post.id|Acc];
                  [Post] when Mode == recursive ->
                      Replies = do_lookup_post_ids(Post#post.replies, Mode),
                      [Post#post.id|Acc] ++ Replies
              end
      end, [], PostIds).

%%
%% Insert post
%%

do_insert_post(Post) ->
    case is_valid_insert_post(Post) of
        {true, ParentPost, _TopPost}  ->
            NewPostId =
                case Post#post.id of
                    not_set ->
                        NextPostId = db_meta_db:read_next_post_id(),
                        ?i2b(NextPostId);
                    PostId ->
                        PostId
                end,
            UpdatedAttachments = move_tmp_attachments(Post#post.attachments, NewPostId),
            UpdatedPost = Post#post{
                            id = NewPostId,
                            created = db:seconds_since_epoch(Post#post.created),
                            attachments = UpdatedAttachments},
            ok = insert_and_inform(UpdatedPost),
            case ParentPost of
                not_set ->
                    {ok, UpdatedPost};
                #post{replies = Replies} ->
                    UpdatedParentPost = ParentPost#post{replies = Replies ++ [NewPostId]},
                    ok = insert_and_inform(UpdatedParentPost),
                    ok = update_parent_count(ParentPost#post.id, 1),
                    {ok, UpdatedPost}
            end;
        false ->
            {error, invalid_post}
    end.

is_valid_insert_post(#post{id = not_set} = Post) ->
    check_insert_post(Post);
is_valid_insert_post(Post) ->
    case dets:lookup(?POST_DB, Post#post.id) of
        [] ->
            check_insert_post(Post);
        _ ->
            false
    end.

check_insert_post(#post{title = Title,
                        parent_post_id = not_set,
                        top_post_id = not_set,
                        created = Created})
  when Title /= not_set andalso
       (Created == not_set orelse is_integer(Created)) ->
    {true, not_set, not_set};
check_insert_post(#post{title = not_set,
                        parent_post_id = ParentPostId,
                        top_post_id = TopPostId,
                        created = Created})
  when ParentPostId /= not_set andalso
       TopPostId /= not_set andalso
       (Created == not_set orelse is_integer(Created)) ->
    case dets:lookup(?POST_DB, ParentPostId) of
        [ParentPost] ->
            case dets:lookup(?POST_DB, TopPostId) of
                [TopPost] ->
                    {true, ParentPost, TopPost};
                [] ->
                    false
            end;
        [] ->
            false
    end;
check_insert_post(_) ->
    false.

move_tmp_attachments(TmpAttachments, NewPostId) ->
    NewPath = filename:join([?BESPOKE_ATTACHMENT_PATH, NewPostId]),
    ok = file:make_dir(NewPath),
    TmpPath = ?BESPOKE_TMP_PATH,
    lists:map(
      fun({TmpAttachment, ContentType}) ->
              TmpFilePath = filename:join([TmpPath, TmpAttachment]),
              NewAttachment = string:trim(TmpAttachment, leading, "0123456789-"),
              NewFilePath = filename:join([NewPath, NewAttachment]),
              ?log_info("Moving ~s to ~s", [TmpFilePath, NewFilePath]),
              ok = file:rename(TmpFilePath, NewFilePath),
              {NewAttachment, ContentType}
      end, TmpAttachments).

update_parent_count(not_set, _N) ->
    ok;
update_parent_count(PostId, N) ->
    [Post] = dets:lookup(?POST_DB, PostId),
    ok = insert_and_inform(Post#post{reply_count = Post#post.reply_count + N}),
    update_parent_count(Post#post.parent_post_id, N).

%%
%% Delete all posts (recursively)
%%

delete_all([]) ->
    0;
delete_all([PostId|Rest]) ->
    [Post] = dets:lookup(?POST_DB, PostId),
    ok = delete_and_inform(PostId),
    delete_all(Post#post.replies) + delete_all(Rest) + 1.

%%
%% Subscription handling
%%

insert_and_inform(Post) when is_record(Post, post) ->
    ok = dets:insert(?POST_DB, Post),
    inform_subscribers(Post#post.id).

delete_and_inform(PostId) ->
    ok = dets:delete(?POST_DB, PostId),
    inform_subscribers(PostId).

inform_subscribers(PostId) ->
    ets:foldl(
      fun(#subscription{id = SubscriptionId,
                        subscriber = Subscriber,
                        monitor_ref = MonitorRef,
                        post_ids = PostIds}, Acc) ->
              case lists:member(PostId, PostIds) of
                  true ->
                      Subscriber ! {subscription_change, SubscriptionId, PostId},
                      true = demonitor(MonitorRef),
                      true = ets:delete(?SUBSCRIPTION_DB, SubscriptionId),
                      Acc;
                  false ->
                      Acc
              end
      end, ok, ?SUBSCRIPTION_DB).
