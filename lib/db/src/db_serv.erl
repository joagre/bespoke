-module(db_serv).
-export([start_link/0, stop/0]).
-export([get_user_id/0,
         list_top_posts/0,
         lookup_posts/1, lookup_posts/2, lookup_post_ids/1, lookup_post_ids/2,
         insert_post/1,
         delete_post/1,
         toggle_like/2,
         subscribe_on_changes/1]).
-export([sync/0]).
-export([message_handler/1]).
-export_type([user_id/0, post_id/0, title/0, body/0, author/0,
              seconds_since_epoch/0, attachment_path/0, content_type/0,
              subscription_id/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("apptools/include/serv.hrl").
-include("../include/db.hrl").

-define(POST_DB_FILENAME, "/var/tmp/bespoke/db/post.db").
-define(POST_DB, post).
-define(META_DB_FILENAME, "/var/tmp/bespoke/db/meta.db").
-define(META_DB, meta).
-define(SUBSCRIPTION_DB, db_serv_subscription).
-define(BESPOKE_ATTACHMENTS_PATH, "/var/tmp/bespoke/attachment").
-define(BESPOKE_ATTACHMENTS_TMP_PATH, "/var/tmp/bespoke/attachment/tmp").

-type user_id() :: integer().
-type post_id() :: binary().
-type title() :: binary().
-type body() :: binary().
-type author() :: binary().
-type seconds_since_epoch() :: integer().
-type attachment_path() :: binary().
-type content_type() :: binary().
-type monitor_ref() :: reference().
-type subscription_id() :: reference().

-record(state, {
                parent :: pid(),
                next_user_id = 0 :: integer(),
                next_post_id = 0 :: integer()
               }).

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
    ?spawn_server(fun init/1, fun ?MODULE:message_handler/1,
                  #serv_options{name = ?MODULE}).

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
%% Exported: list_top_posts
%%

-spec list_top_posts() -> [#post{}].

list_top_posts() ->
    serv:call(?MODULE, list_top_posts).

%%
%% Exported: lookup_posts
%%

-spec lookup_posts([post_id()], flat | recursive) -> [#post{}].

lookup_posts(PostIds) ->
    lookup_posts(PostIds, flat).

lookup_posts(PostIds, Mode) ->
    serv:call(?MODULE, {lookup_posts, PostIds, Mode}).

%%
%% Exported: lookup_post_ids
%%

-spec lookup_post_ids([post_id()], flat | recursive) -> [post_id()].

lookup_post_ids(PostIds) ->
    lookup_posts(PostIds, flat).

lookup_post_ids(PostIds, Mode) ->
    serv:call(?MODULE, {lookup_post_ids, PostIds, Mode}).

%%
%% Exported: insert_post
%%

-spec insert_post(#post{}) -> {ok, #post{}} | {error, invalid_post}.

insert_post(Post) ->
    serv:call(?MODULE, {insert_post, Post}).

%%
%% delete_post
%%

-spec delete_post(post_id()) -> ok | {error, not_found}.

delete_post(PostId) ->
    serv:call(?MODULE, {delete_post, PostId}).

%%
%% Exported: sync
%%

-spec sync() -> ok.

sync() ->
    serv:call(?MODULE, sync).

%%
%% Exported: toggle_like
%%

-spec toggle_like(post_id(), user_id()) ->
          {ok, [user_id()]} | {error, not_found}.

toggle_like(PostId, UserId) ->
    serv:call(?MODULE, {toggle_like, PostId, UserId}).

%%
%% Exported: subscribe_on_changes
%%

-spec subscribe_on_changes([post_id()]) -> subscription_id().

subscribe_on_changes(PostIds) ->
    serv:call(?MODULE, {subscribe_on_changes, self(), PostIds}).

%%
%% Server
%%

init(Parent) ->
    {ok, ?POST_DB} =
        dets:open_file(
          ?POST_DB, [{file, ?POST_DB_FILENAME}, {keypos, #post.id}]),
    {ok, ?META_DB} =
        dets:open_file(
          ?META_DB, [{file, ?META_DB_FILENAME}, {keypos, #meta.type}]),
    case dets:lookup(?META_DB, basic) of
        [] ->
            Meta = #meta{},
            ok = dets:insert(?META_DB, Meta);
        [Meta] ->
            ok
    end,
    ?SUBSCRIPTION_DB =
        ets:new(?SUBSCRIPTION_DB, [{keypos, #subscription.id}, named_table]),
    ?log_info("Database server has been started"),
    {ok, #state{parent = Parent,
                next_user_id = Meta#meta.next_user_id,
                next_post_id = Meta#meta.next_post_id}}.

message_handler(S) ->
    receive
        {call, From, stop = Call} ->
            ?log_debug("Call: ~p", [Call]),
            _ = dets:close(?META_DB),
            _ = dets:close(?POST_DB),
            {reply, From, ok};
        {call, From, get_user_id = Call} ->
            ?log_debug("Call: ~p", [Call]),
            [#meta{next_user_id = NextUserId} = Meta] =
                dets:lookup(?META_DB, basic),
            UpdatedMeta = Meta#meta{next_user_id = NextUserId + 1},
            ok = dets:insert(?META_DB, UpdatedMeta),
            {reply, From, NextUserId};
        {call, From, list_top_posts = Call} ->
            ?log_debug("Call: ~p", [Call]),
            TopPosts =
                dets:match_object(
                  ?POST_DB, #post{top_post_id = not_set, _ = '_'}),
            SortedTopPosts =
                lists:sort(
                  fun(PostA, PostB) ->
                          PostA#post.created > PostB#post.created
                  end, TopPosts),
            {reply, From, SortedTopPosts};
        {call, From, {lookup_posts, PostIds, Mode} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, do_lookup_posts(PostIds, Mode)};
        {call, From, {lookup_post_ids, PostIds, Mode} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, do_lookup_post_ids(PostIds, Mode)};
        {call, From, {insert_post, Post} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case do_insert_post(S#state.next_post_id, Post) of
                {ok, NextUpcomingPostId, InsertedPost} ->
                    {reply, From, {ok, InsertedPost},
                     S#state{next_post_id = NextUpcomingPostId}};
                {error, Reason} ->
                    {reply, From, {error, Reason}}
            end;
        {call, From, {delete_post, PostId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:lookup(?POST_DB, PostId) of
                [#post{parent_post_id = ParentPostId}]
                  when ParentPostId /= not_set ->
                    [ParentPost] = dets:lookup(?POST_DB, ParentPostId),
                    ok = insert_and_inform(
                           ParentPost#post{
                             replies = lists:delete(
                                         PostId,
                                         ParentPost#post.replies)}),
                    N = delete_all([PostId]),
                    ok = update_parent_count(ParentPost#post.id, -N),
                    {reply, From, ok};
                [_TopPost] ->
                    _N = delete_all([PostId]),
                    {reply, From, ok};
                [] ->
                    {reply, From, {error, not_found}}
            end;
        {call, From, sync = Call} ->
            ?log_debug("Call: ~p", [Call]),
            ok = dets:sync(?POST_DB),
            {reply, From, ok};
        {call, From, {toggle_like, PostId, UserId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:lookup(?POST_DB, PostId) of
                [#post{likers = Likers} = Post] ->
                    UpdatedLikers =
                        case lists:member(UserId, Likers) of
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
        {call, From, {subscribe_on_changes, Subscriber, PostIds} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            SubscriptionId = make_ref(),
            true = ets:insert(?SUBSCRIPTION_DB,
                              #subscription{
                                 id = SubscriptionId,
                                 subscriber = Subscriber,
                                 monitor_ref = monitor(process, Subscriber),
                                 post_ids = PostIds}),
            {reply, From, SubscriptionId};
        {'DOWN', MonitorRef, process, Pid, _Reason} ->
            ?log_info("Subscriber died: ~w", [Pid]),
            true = ets:match_delete(
                     ?SUBSCRIPTION_DB,
                     #subscription{monitor_ref = MonitorRef, _ = '_'}),
            noreply;
        {'EXIT', Pid, Reason} when S#state.parent == Pid ->
            exit(Reason);
        {system, From, Request} ->
            ?log_debug("System: ~p", [Request]),
            {system, From, Request};
        UnknownMessage ->
            ?log_error("Unknown message: ~p", [UnknownMessage]),
            noreply
    end.

%%
%% Insert post
%%

do_insert_post(NextPostId, Post) ->
    case is_valid_insert_post(Post) of
        {true, ParentPost, _TopPost}  ->
            case Post#post.id of
                not_set ->
                    NextUpcomingPostId = NextPostId + 1,
                    UpdatedMeta =
                        #meta{type = basic,
                              next_post_id = NextUpcomingPostId},
                    ok = dets:insert(?META_DB, UpdatedMeta),
                    NewPostId = ?i2b(NextPostId);
                PostId ->
                    NewPostId = PostId,
                    NextUpcomingPostId = NextPostId
            end,
            UpdatedAttachments =
                move_tmp_attachments(Post#post.attachments, NewPostId),
            UpdatedPost =
                Post#post{
                  id = NewPostId,
                  created = seconds_since_epoch(Post#post.created),
                  attachments = UpdatedAttachments},
            ok = insert_and_inform(UpdatedPost),
            case ParentPost of
                not_set ->
                    {ok, NextUpcomingPostId, UpdatedPost};
                #post{replies = Replies} ->
                    UpdatedParentPost =
                        ParentPost#post{
                          replies = Replies ++ [NewPostId]},
                    ok = insert_and_inform(UpdatedParentPost),
                    ok = update_parent_count(ParentPost#post.id, 1),
                    {ok, NextUpcomingPostId, UpdatedPost}
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
    NewPath = filename:join([?BESPOKE_ATTACHMENTS_PATH, NewPostId]),
    ok = file:make_dir(NewPath),
    TmpPath = ?BESPOKE_ATTACHMENTS_TMP_PATH,
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
                          Replies =
                              do_lookup_posts(Post#post.replies, Mode),
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
                      Replies =
                          do_lookup_post_ids(Post#post.replies, Mode),
                      [Post#post.id|Acc] ++ Replies
              end
      end, [], PostIds).

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
%% Utilities
%%

seconds_since_epoch(not_set) ->
    os:system_time(second);
seconds_since_epoch(Seconds) ->
    Seconds.
