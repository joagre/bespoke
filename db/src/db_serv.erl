-module(db_serv).
-export([start_link/0, stop/0]).
-export([list_top_posts/0,
         lookup_posts/1, lookup_posts/2,
         insert_post/1,
         delete_post/1]).
-export([sync/0]).
-export([message_handler/1]).
-export_type([post_id/0, title/0, body/0, author/0, seconds_since_epoch/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("apptools/include/serv.hrl").
-include("db.hrl").

-define(POST_DB_FILENAME, "posts.db").
-define(POST_DB, posts).
-define(META_DB_FILENAME, "meta.db").
-define(META_DB, meta).

-type post_id() :: binary().
-type title() :: binary().
-type body() :: binary().
-type author() :: binary().
-type seconds_since_epoch() :: integer().

-record(state, {
                parent :: pid(),
                next_post_id = 0 :: integer()
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
%% Server
%%

init(Parent) ->
    {ok, ?POST_DB} =
        dets:open_file(
          ?POST_DB,
          [{file, filename:join(code:priv_dir(db), ?POST_DB_FILENAME)},
           {keypos, #post.id}]),
    {ok, ?META_DB} =
        dets:open_file(
          ?META_DB,
          [{file, filename:join(code:priv_dir(db), ?META_DB_FILENAME)},
           {keypos, #meta.type}]),
    case dets:lookup(?META_DB, basic) of
        [] ->
            ok = dets:insert(?META_DB, #meta{type = basic,
                                             next_post_id = 0}),
            NextPostId = 0;
        [#meta{next_post_id = NextPostId}] ->
            ok
    end,
    ?log_info("Database server has been started"),
    {ok, #state{parent = Parent, next_post_id = NextPostId}}.

message_handler(S) ->
    receive
        {call, From, stop = Call} ->
            ?log_debug("Call: ~p", [Call]),
            ok = dets:close(?META_DB),
            ok = dets:close(?POST_DB),
            {reply, From, ok};
        {call, From, list_top_posts = Call} ->
            ?log_debug("Call: ~p", [Call]),
            TopPosts =
                dets:match_object(
                  ?POST_DB, #post{top_post_id = not_set, _ = '_'}),
            SortedTopPosts =
                lists:sort(
                  fun(PostA, PostB) ->
                          PostA#post.created =< PostB#post.created
                  end, TopPosts),
            {reply, From, SortedTopPosts};
        {call, From, {lookup_posts, PostIds, Mode} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {reply, From, do_lookup_posts(PostIds, Mode)};
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
                    ok = dets:insert(
                           ?POST_DB,
                           ParentPost#post{
                             replies = lists:delete(
                                         PostId,
                                         ParentPost#post.replies)}),
                    N = delete_all([PostId]),
                    ok = update_parent_count(ParentPost#post.id, -N),
                    {reply, From, ok};
                [_TopPost] ->
                    _ = delete_all([PostId]),
                    {reply, From, ok};
                [] ->
                    {reply, From, {error, not_found}}
            end;
        {call, From, sync = Call} ->
            ?log_debug("Call: ~p", [Call]),
            ok = dets:sync(?POST_DB),
            {reply, From, ok};
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
            UpdatedPost =
                Post#post{
                  id = NewPostId,
                  created = seconds_since_epoch(Post#post.created)},
            ok = dets:insert(?POST_DB, UpdatedPost),
            case ParentPost of
                not_set ->
                    {ok, NextUpcomingPostId, UpdatedPost};
                #post{replies = Replies} ->
                    UpdatedParentPost =
                        ParentPost#post{
                          replies = Replies ++ [NewPostId]},
                    ok = dets:insert(?POST_DB, UpdatedParentPost),
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

update_parent_count(not_set, _N) ->
    ok;
update_parent_count(PostId, N) ->
    [Post] = dets:lookup(?POST_DB, PostId),
    ok = dets:insert(?POST_DB,
                     Post#post{
                       reply_count = Post#post.reply_count + N}),
    update_parent_count(Post#post.parent_post_id, N).

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

%%
%% Delete all posts (recursively)
%%

delete_all([]) ->
    0;
delete_all([PostId|Rest]) ->
    [Post] = dets:lookup(?POST_DB, PostId),
    ok = dets:delete(?POST_DB, PostId),
    delete_all(Post#post.replies) + delete_all(Rest) + 1.

%%
%% Utilities
%%

seconds_since_epoch(not_set) ->
    os:system_time(second);
seconds_since_epoch(Seconds) ->
    Seconds.
