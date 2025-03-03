% -*- fill-column: 100; -*-

-module(db_post_db).
-export([open/0, dump/0, sync/0, close/0, create_post/1, read_top_posts/0, read_posts/2,
         read_post_ids/2, delete_post/1, toggle_post_like/2]).

-include_lib("apptools/include/shorthand.hrl").
-include_lib("apptools/include/log.hrl").
-include("../include/db.hrl").

-define(POST_FILE_PATH, filename:join(?BESPOKE_DB_DIR, "post.db")).
-define(POST_DB, post_db).

%%
%% Exported: open
%%

-spec open() -> ok.

open() ->
    {ok, _} = dets:open_file(?POST_DB, [{file, ?POST_FILE_PATH}, {keypos, #post.id}]),
    ok.

%%
%% Exported: dump
%%

-spec dump() -> [{dets:tab_name(), [#post{}]}].

dump() ->
    [{?POST_DB, db:dets_dump(?POST_DB)}].

%%
%% Exported: sync
%%

-spec sync() -> ok | {error, term()}.

sync() ->
    dets:sync(?POST_DB).

%%
%% Exported: close
%%

-spec close() -> ok | {error, term()}.

close() ->
    dets:close(?POST_DB).

%%
%% Exported: create_post
%%

-spec create_post(#post{}) -> {ok, #post{}} | {error, invalid_post}.

create_post(Post) ->
    case is_valid_post(Post) of
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
            UpdatedPost = Post#post{id = NewPostId,
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

is_valid_post(#post{id = not_set} = Post) ->
    check_post(Post);
is_valid_post(Post) ->
    case dets:lookup(?POST_DB, Post#post.id) of
        [] ->
            check_post(Post);
        _ ->
            false
    end.

check_post(#post{title = Title,
                 parent_post_id = not_set,
                 top_post_id = not_set,
                 created = Created})
  when Title /= not_set andalso
       (Created == not_set orelse is_integer(Created)) ->
    {true, not_set, not_set};
check_post(#post{title = not_set,
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
check_post(_) ->
    false.

move_tmp_attachments(TmpAttachments, NewPostId) ->
    NewPath = filename:join([?BESPOKE_POST_PATH, NewPostId]),
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

%%
%% Exported: read_top_posts
%%

-spec read_top_posts() -> [#post{}].

read_top_posts() ->
    sort_posts(dets:match_object(?POST_DB, #post{top_post_id = not_set, _ = '_'})).

%%
%% Exported: read_posts
%%

-spec read_posts([db:post_id()], flat | recursive) -> [#post{}].

read_posts(PostIds, Mode) ->
    Posts =
        lists:foldr(
          fun(PostId, Acc) ->
                  case dets:lookup(?POST_DB, PostId) of
                      [Post] when Mode == flat ->
                          [Post|Acc];
                      [Post] when Mode == recursive ->
                          Replies = read_posts(Post#post.replies, Mode),
                          [Post|Acc] ++ Replies
                  end
          end, [], PostIds),
    sort_posts(Posts).

%%
%% Exported: read_post_ids
%%

-spec read_post_ids([db:post_id()], flat | recursive) -> [db:post_id()].

read_post_ids(PostIds, Mode) ->
    lists:foldr(fun(PostId, Acc) ->
                        case dets:lookup(?POST_DB, PostId) of
                            [Post] when Mode == flat ->
                                [Post#post.id|Acc];
                            [Post] when Mode == recursive ->
                                Replies = read_post_ids(Post#post.replies, Mode),
                                [Post#post.id|Acc] ++ Replies
                        end
                end, [], PostIds).

%%
%% Exported: delete_post
%%

-spec delete_post(db:post_id()) -> ok | {error, not_found}.

delete_post(PostId) ->
    case dets:lookup(?POST_DB, PostId) of
        [#post{parent_post_id = ParentPostId}] when ParentPostId /= not_set ->
            [ParentPost] = dets:lookup(?POST_DB, ParentPostId),
            ok = insert_and_inform(
                   ParentPost#post{replies = lists:delete(PostId, ParentPost#post.replies)}),
            N = delete_all([PostId]),
            update_parent_count(ParentPost#post.id, -N);
        [_TopPost] ->
            _N = delete_all([PostId]),
            ok;
        [] ->
            {error, not_found}
    end.

delete_all([]) ->
    0;
delete_all([PostId|Rest]) ->
    [Post] = dets:lookup(?POST_DB, PostId),
    ok = delete_and_inform(PostId),
    delete_all(Post#post.replies) + delete_all(Rest) + 1.

%%
%% Exported: toggle_post_like
%%

-spec toggle_post_like(db:post_id(), db:user_id()) -> {ok, [db:user_id()]} | {error, not_found}.

toggle_post_like(PostId, UserId) ->
    case dets:lookup(?POST_DB, PostId) of
        [#post{likers = Likers} = Post] ->
            UpdatedLikers = case lists:member(UserId, Likers) of
                                true ->
                                    lists:delete(UserId, Likers);
                                false ->
                                    [UserId|Likers]
                            end,
            ok = insert_and_inform(Post#post{likers = UpdatedLikers}),
            {ok, UpdatedLikers};
        [] ->
            {error, not_found}
    end.

%%
%% Utilities
%%

sort_posts(Posts) ->
    lists:sort(fun(PostA, PostB) ->
                       PostA#post.created =< PostB#post.created
               end, Posts).

delete_and_inform(PostId) ->
    ok = dets:delete(?POST_DB, PostId),
    db_subscription_db:inform_subscribers(PostId).

insert_and_inform(Post) when is_record(Post, post) ->
    ok = dets:insert(?POST_DB, Post),
    db_subscription_db:inform_subscribers(Post#post.id).

update_parent_count(not_set, _N) ->
    ok;
update_parent_count(PostId, N) ->
    [Post] = dets:lookup(?POST_DB, PostId),
    ok = insert_and_inform(Post#post{reply_count = Post#post.reply_count + N}),
    update_parent_count(Post#post.parent_post_id, N).
