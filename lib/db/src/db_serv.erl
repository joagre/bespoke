-module(db_serv).
-export([start_link/0, stop/0]).
-export([get_user_id/0]).
-export([list_top_posts/0, lookup_posts/1, lookup_posts/2, lookup_post_ids/1,
         lookup_post_ids/2, insert_post/1, delete_post/1, toggle_like/2]).
-export([list_files/0, lookup_files/1, insert_file/1, delete_file/1,
         file_uploaded/1]).
-export([subscribe_on_changes/1]).
-export([sync/0]).
-export([open_disk_db/3, open_disk_index_db/2, open_ram_db/2]).
-export([message_handler/1]).
-export_type([ssid/0, host/0, user_id/0, username/0, message_id/0,
              message_attachment_id/0, post_id/0, title/0, body/0,
              seconds_since_epoch/0, attachment_path/0, content_type/0,
              file_id/0, filename/0, file_size/0, subscription_id/0,
              monitor_ref/0]).

-include_lib("kernel/include/file.hrl").
-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("apptools/include/serv.hrl").
-include("../include/db.hrl").

-define(META_DB_FILENAME, filename:join(?DB_DIR, "meta.db")).
-define(META_DB, meta).

-define(MESSAGE_DB_FILENAME, filename:join(?DB_DIR, "message.db")).
-define(MESSAGE_DB, message).

-define(MESSAGE_RECIPIENT_DB_FILENAME,
        filename:join(?DB_DIR, "message_recipient.db")).
-define(MESSAGE_RECIPIENT_DB, message_recipient).

-define(MESSAGE_ATTACHMENT_DB_FILENAME,
        filename:join(?DB_DIR, "message_attachment.db")).
-define(MESSAGE_ATTACHMENT_DB, message_attachment).

-define(POST_DB_FILENAME, filename:join(?DB_DIR, "post.db")).
-define(POST_DB, post).

-define(FILE_DB_FILENAME, filename:join(?DB_DIR, "file.db")).
-define(FILE_DB, file).

-define(SUBSCRIPTION_DB, db_serv_subscription).

-define(BESPOKE_TMP_PATH, filename:join(?RUNTIME_DIR, "tmp")).
-define(BESPOKE_MESSAGE_PATH, filename:join(?RUNTIME_DIR, "message")).
-define(BESPOKE_ATTACHMENT_PATH, filename:join(?RUNTIME_DIR, "attachment")).
-define(BESPOKE_FILE_PATH, filename:join(?RUNTIME_DIR, "file")).

-type ssid() :: binary().
-type host() :: binary().

-type user_id() :: integer().
-type username() :: binary().

-type message_id() :: integer().
-type message_attachment_id() :: integer().

-type post_id() :: binary().
-type title() :: binary().
-type body() :: binary().
-type seconds_since_epoch() :: integer().
-type attachment_path() :: binary().
-type content_type() :: binary().

-type file_id() :: integer().
-type filename() :: binary().
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
%% Exported: toggle_like
%%

-spec toggle_like(post_id(), user_id()) ->
          {ok, [user_id()]} | {error, not_found}.

toggle_like(PostId, UserId) ->
    serv:call(?MODULE, {toggle_like, PostId, UserId}).

%%
%% Exported: list_files
%%

-spec list_files() -> [#file{}].

list_files() ->
    serv:call(?MODULE, list_files).

%%
%% Exported: lookup_files
%%

-spec lookup_files([file_id()]) -> [#file{}].

lookup_files(FileIds) ->
    serv:call(?MODULE, {lookup_files, FileIds}).

%%
%% Exported: insert_file
%%

-spec insert_file(#file{}) -> {ok, #file{}} | {error, invalid_file}.

insert_file(File) ->
    serv:call(?MODULE, {insert_file, File}).

%%
%% delete_file
%%

-spec delete_file(file_id()) -> ok | {error, not_found}.

delete_file(FileId) ->
    serv:call(?MODULE, {delete_file, FileId}).

%%
%% file_uploaded
%%

-spec file_uploaded(file_id()) -> ok | {error, not_found}.

file_uploaded(FileId) ->
    serv:call(?MODULE, {file_uploaded, FileId}).

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
%% Exported: open_disk_db
%%

open_disk_db(Name, Filename, KeyPos) ->
    {ok, Name} = dets:open_file(Name, [{file, Filename}, {keypos, KeyPos}]),
    case Name of
        ?META_DB ->
            case dets:lookup(?META_DB, basic) of
                [] ->
                    dets:insert(?META_DB, #meta{});
                [_] ->
                    ok
            end;
        _ ->
            ok
    end.

%%
%% Exported: open_disk_index_db
%%

open_disk_index_db(Name, Filename) ->
    {ok, Name} = apptools_persistent_index:open(Name, Filename),
    ok.

%%
%% Exported: open_ram_db

open_ram_db(Name, KeyPos) ->
    Name = ets:new(Name, [{keypos, KeyPos}, named_table, public]),
    ok.

%%
%% Server
%%

init(Parent) ->
    ok = open_disk_db(?META_DB, ?META_DB_FILENAME, #meta.type),
    ok = open_disk_db(?MESSAGE_DB, ?MESSAGE_DB_FILENAME, #message.id),
    ok = open_disk_db(?MESSAGE_RECIPIENT_DB, ?MESSAGE_RECIPIENT_DB_FILENAME,
                      #message_recipient.message_id),



    ok = open_disk_db(?POST_DB, ?POST_DB_FILENAME, #post.id),
    ok = open_disk_db(?FILE_DB, ?FILE_DB_FILENAME, #file.id),
    ok = open_ram_db(?SUBSCRIPTION_DB, #subscription.id),
    ?log_info("Database server has been started"),
    {ok, #state{parent = Parent}}.

message_handler(S) ->
    receive
        {call, From, stop = Call} ->
            ?log_debug("Call: ~p", [Call]),
            _ = dets:close(?FILE_DB),
            _ = dets:close(?POST_DB),
            _ = dets:close(?MESSAGE_DB),
            _ = dets:close(?META_DB),
            {reply, From, ok};
        {call, From, get_user_id = Call} ->
            ?log_debug("Call: ~p", [Call]),
            NextUserId = step_meta_id(#meta.next_user_id),
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
            {reply, From, do_insert_post(Post)};
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
        {call, From, list_files = Call} ->
            ?log_debug("Call: ~p", [Call]),
            Files =
                dets:foldl(fun(#file{is_uploading = true} = File, Acc) ->
                                   [update_uploaded_size(File)|Acc];
                              (File, Acc) ->
                                   [File|Acc]
                           end, [], ?FILE_DB),
            SortedFiles =
                lists:sort(fun(FileA, FileB) ->
                                   FileA#file.created > FileB#file.created
                           end, Files),
            {reply, From, SortedFiles};
        {call, From, {lookup_files, FileIds} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            Files =
                lists:foldr(
                  fun(FileId, Acc) ->
                          [File] = dets:lookup(?FILE_DB, FileId),
                          [update_uploaded_size(File)|Acc]
                  end, [], FileIds),
            SortedFiles =
                lists:sort(
                  fun(FileA, FileB) ->
                          FileA#file.created > FileB#file.created
                  end, Files),
            {reply, From, SortedFiles};
        {call, From, {insert_file, File} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            NextFileId = step_meta_id(#meta.next_file_id),
            InsertedFile =
                File#file{
                  id = NextFileId,
                  created = seconds_since_epoch()
                 },
            ok = dets:insert(?FILE_DB, InsertedFile),
            {reply, From, {ok, InsertedFile}};
        {call, From, {delete_file, FileId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:lookup(?FILE_DB, FileId) of
                [File] ->
                    ok = dets:delete(?FILE_DB, FileId),
                    ok = delete_file_on_disk(File),
                    {reply, From, ok};
                [] ->
                    {reply, From, {error, not_found}}
            end;
        {call, From, {file_uploaded, FileId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:lookup(?FILE_DB, FileId) of
                [File] ->
                    UpdatedFile = File#file{is_uploading = false},
                    ok = dets:insert(?FILE_DB, UpdatedFile),
                    ok = move_file_on_disk(UpdatedFile),
                    {reply, From, ok};
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
        {call, From, sync = Call} ->
            ?log_debug("Call: ~p", [Call]),
            ok = dets:sync(?META_DB),
            ok = dets:sync(?POST_DB),
            ok = dets:sync(?MESSAGE_DB),
            ok = dets:sync(?FILE_DB),
            {reply, From, ok};
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
%% Insert post
%%

do_insert_post(Post) ->
    case is_valid_insert_post(Post) of
        {true, ParentPost, _TopPost}  ->
            NewPostId =
                case Post#post.id of
                    not_set ->
                        NextPostId = step_meta_id(#meta.next_post_id),
                        ?i2b(NextPostId);
                    PostId ->
                        PostId
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
                    {ok, UpdatedPost};
                #post{replies = Replies} ->
                    UpdatedParentPost =
                        ParentPost#post{
                          replies = Replies ++ [NewPostId]},
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

%%
%% List files
%%

update_uploaded_size(#file{id = FileId, is_uploading = true} = File) ->
    TmpPath = ?BESPOKE_TMP_PATH,
    TmpFilePathBeginning =
        filename:join([TmpPath, "file-" ++ io_lib:format("~w-", [FileId])]),
    case filelib:wildcard(TmpFilePathBeginning ++ "*") of
        [TmpFilePath|_] ->
            case file:read_file_info(TmpFilePath) of
                {ok, FileInfo} ->
                    File#file{uploaded_size = FileInfo#file_info.size};
                {error, _} ->
                    File
            end;
        [] ->
            File
    end;
update_uploaded_size(File) ->
    File.

%%
%% Delete file
%%

delete_file_on_disk(#file{id = FileId, filename = Filename}) ->
    TmpPath = ?BESPOKE_TMP_PATH,
    TmpFilePathBeginning =
        filename:join([TmpPath, io_lib:format("file-~w-", [FileId])]),
    case filelib:wildcard(TmpFilePathBeginning ++ "*") of
        [TmpFilePath] ->
            ?log_info("Deleting ~s (if it exists)...", [TmpFilePath]),
            _ = file:delete(TmpFilePath),
            UploadedFilePath =
                filename:join(
                  [?BESPOKE_FILE_PATH,
                   io_lib:format("~w-~s", [FileId, Filename])]),
            ?log_info("Deleting ~s (if it exists)...", [UploadedFilePath]),
            _ = file:delete(UploadedFilePath),
            ok;
        [] ->
            ok
    end.

%%
%% File uploaded
%%

move_file_on_disk(#file{id = FileId, filename = Filename}) ->
    TmpPath = ?BESPOKE_TMP_PATH,
    TmpFilePathBeginning =
        filename:join([TmpPath, io_lib:format("file-~w-", [FileId])]),
    case filelib:wildcard(TmpFilePathBeginning ++ "*") of
        [TmpFilePath] ->
            UploadedFilePath =
                filename:join(
                  [?BESPOKE_FILE_PATH,
                   io_lib:format("~w-~s", [FileId, Filename])]),
            ?log_info("Moving ~s to ~s", [TmpFilePath, UploadedFilePath]),
            ok = file:rename(TmpFilePath, UploadedFilePath);
        [] ->
            ok
    end.

%%
%% Utilities
%%

step_meta_id(N) ->
    [Meta] = dets:lookup(?META_DB, basic),
    NextId = element(N, Meta),
    UpdatedMeta = setelement(N, Meta, NextId + 1),
    ok = dets:insert(?META_DB, UpdatedMeta),
    NextId.

seconds_since_epoch() ->
    os:system_time(second).

seconds_since_epoch(not_set) ->
    os:system_time(second);
seconds_since_epoch(Seconds) ->
    Seconds.
