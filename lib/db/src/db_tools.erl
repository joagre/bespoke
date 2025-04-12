% -*- fill-column: 100; -*-

-module(db_tools).
-compile(export_all).
-export([create_subreddit_db/0, purge_subreddit_db/0, dump_subreddit_db/0]).

-include("../include/db.hrl").
-include("../../apptools/include/log.hrl").
-include("../../apptools/include/shorthand.hrl").

-define(SUBREDDIT, filename:join(code:priv_dir(db), "reddit/sweden")).
-define(SUBMISSIONS, "/media/jocke/EXTERNSL/reddit/subreddits23/sweden_submissions_sorted").
-define(COMMENTS, "/media/jocke/EXTERNSL/reddit/subreddits23/sweden_comments_sorted").

%%
%% Exported: dump_subreddit
%%

%% Example usage:
%%
%% (bespoke@localhost)5> db_tools:purge_subreddit_db().
%% ok
%% (bespoke@localhost)6> db_tools:create_subreddit_db().
%% ** Creating new submission database
%% ** Total number of submissions = 10
%% ** Parsed 310 submissions
%% ** Creating new comment database
%% ** Total number of parentd ids = 268
%% ** Total number of comments = 440
%% ** Parsed 9574215 comments

create_subreddit_db() ->
    %% * SubsmissionDb is a dets set table with submission in JSON
    %%   object format keyed on id
    %% * CommentDb is a dets bag table with comments in JSON object
    %%   format  keyed on parent_id
    %% Hint: Read about the JSON object format in the Reddit API
    {SubmissionDb, CommentDb} =
        get_cached_subreddit(
          ?SUBMISSIONS,
          ?COMMENTS,
          _NoPlainSubmissions = 10,
          _NoGallerySubmissions = 5,
          _NoVideoSubmissions = 5,
          _MinNoGalleryImages = 2,
          _MinNoComments = 20),
    [] = insert_subreddit(SubmissionDb, CommentDb),
    close_dbs(SubmissionDb, CommentDb).

get_cached_subreddit(SubmissionsFile, CommentsFile,  NoPlainSubmissions,  NoGallerySubmissions,
                     NoVideoSubmissions, MinNoGalleryImages, MinNoComments) ->
    SubmissionsDbFile = filename:join(?SUBREDDIT, "submissions.db"),
    CommentsDbFile = filename:join(?SUBREDDIT, "comments.db"),
    %% If there is no submission database just delete the comments
    %% database as well (if it exists)
    case filelib:is_regular(SubmissionsDbFile) of
        true ->
            ok;
        false ->
            case filelib:is_regular(CommentsDbFile) of
                true ->
                    io:format("** Deleting comment database~n"),
                    _ = file:delete(CommentsDbFile);
                false ->
                    ok
            end
    end,
    %% Recreate the databases if they do not exist
    case filelib:is_regular(SubmissionsDbFile) of
        true ->
            case filelib:is_regular(CommentsDbFile) of
                true ->
                    io:format("** Using cached databases~n"),
                    open_dbs(SubmissionsDbFile, CommentsDbFile);
                false ->
                    io:format("** Using cached submission database~n"),
                    {SubmissionDb, CommentDb} = open_dbs(SubmissionsDbFile, CommentsDbFile),
                    io:format("** Creating new comment database~n"),
                    {NoParentsIds, NoComments, ParsedComments} =
                        populate_comment_db(CommentsFile, SubmissionDb, CommentDb),
                    io:format("** Total number of parent ids = ~p~n", [NoParentsIds]),
                    io:format("** Total number of comments = ~p~n", [NoComments]),
                    io:format("** Parsed ~p comments~n", [ParsedComments]),
                    {SubmissionDb, CommentDb}
            end;
        false ->
            {SubmissionDb, CommentDb} = open_dbs(SubmissionsDbFile, CommentsDbFile),
            io:format("** Creating new submission database~n"),
            {NoSubmissions, ParsedSubmissions} =
                populate_submission_db(SubmissionsFile, SubmissionDb, NoPlainSubmissions,
                                       NoGallerySubmissions, NoVideoSubmissions, MinNoGalleryImages,
                                       MinNoComments),
            io:format("** Total number of submissions = ~p~n", [NoSubmissions]),
            io:format("** Parsed ~p submissions~n", [ParsedSubmissions]),
            io:format("** Creating new comment database~n"),
            {NoParentsIds, NoComments, ParsedComments} =
                populate_comment_db(CommentsFile, SubmissionDb, CommentDb),
            io:format("** Total number of parent ids = ~p~n", [NoParentsIds]),
            io:format("** Total number of comments = ~p~n", [NoComments]),
            io:format("** Parsed ~p comments~n", [ParsedComments]),
            {SubmissionDb, CommentDb}
    end.

open_dbs(SubmissionsDbFile, CommentsDbFile) ->
    {ok, SubmissionDb} = dets:open_file(submissions, [{file, SubmissionsDbFile}]),
    {ok, CommentDb} = dets:open_file(comments, [{file, CommentsDbFile}, {type, bag}]),
    {SubmissionDb, CommentDb}.

close_dbs(SubmissionDb, CommentDb) ->
    dets:close(SubmissionDb),
    dets:close(CommentDb).

populate_submission_db(SubmissionsFile, SubmissionDb, NoPlainSubmissions, NoGallerySubmissions,
                       NoVideoSubmissions, MinNoGalleryImages, MinNoComments) ->
    {ok, SubmissionsFd} = file:open(SubmissionsFile, [read, binary]),
    populate_submission_db(SubmissionDb, NoPlainSubmissions, NoGallerySubmissions,
                           NoVideoSubmissions, MinNoGalleryImages, MinNoComments, SubmissionsFd,
                           _ParsedLines = 0).

populate_submission_db(SubmissionDb, 0, 0, 0, _MinNoGalleryImages, _MinNoComments, SubmissionsFd,
                       ParsedLines) ->
    NoSubmissions = dets:info(SubmissionDb, no_keys),
    dets:sync(SubmissionDb),
    file:close(SubmissionsFd),
    {NoSubmissions, ParsedLines};
populate_submission_db(SubmissionDb, NoPlainSubmissions, NoGallerySubmissions, NoVideoSubmissions,
                       MinNoGalleryImages, MinNoComments, SubmissionsFd, ParsedLines) ->
    case file:read_line(SubmissionsFd) of
        {ok, Line} ->
            case parse_submission(Line, MinNoComments) of
                skipped ->
                    populate_submission_db(SubmissionDb, NoPlainSubmissions, NoGallerySubmissions,
                                           NoVideoSubmissions, MinNoGalleryImages, MinNoComments,
                                           SubmissionsFd, ParsedLines + 1);
                #{<<"id">> := Id,
                  <<"is_gallery">> := true,
                  <<"gallery_data">> := #{<<"items">> := Items}} = JsonTerm
                  when NoGallerySubmissions > 0 andalso
                       length(Items) >= MinNoGalleryImages ->
                    ok = dets:insert(SubmissionDb, {Id, JsonTerm}),
                    populate_submission_db(SubmissionDb, NoPlainSubmissions,
                                           NoGallerySubmissions - 1, NoVideoSubmissions,
                                           MinNoGalleryImages, MinNoComments, SubmissionsFd,
                                           ParsedLines + 1);
                #{<<"id">> := Id, <<"is_video">> := true} = JsonTerm
                  when NoVideoSubmissions > 0 ->
                    ok = dets:insert(SubmissionDb, {Id, JsonTerm}),
                    populate_submission_db(SubmissionDb, NoPlainSubmissions,
                                           NoGallerySubmissions, NoVideoSubmissions - 1,
                                           MinNoGalleryImages, MinNoComments, SubmissionsFd,
                                           ParsedLines + 1);
                #{<<"id">> := Id,
                  <<"is_self">> := true} = JsonTerm
                  when NoPlainSubmissions > 0 ->
                    ok = dets:insert(SubmissionDb, {Id, JsonTerm}),
                    populate_submission_db(SubmissionDb, NoPlainSubmissions - 1,
                                           NoGallerySubmissions, NoVideoSubmissions,
                                           MinNoGalleryImages, MinNoComments, SubmissionsFd,
                                           ParsedLines + 1);
                _ ->
                    populate_submission_db(SubmissionDb, NoPlainSubmissions,
                                           NoGallerySubmissions, NoVideoSubmissions,
                                           MinNoGalleryImages, MinNoComments, SubmissionsFd,
                                           ParsedLines + 1)
            end;
        eof ->
            file:close(SubmissionsFd),
            throw({error, eof})
    end.

parse_submission(Line, MinNoComments) ->
    case json:decode(Line) of
        #{<<"num_comments">> := NumComments,
          <<"selftext">> := Selftext} = JsonTerm
          when Selftext /= <<"[deleted]">> andalso
               Selftext /= <<"[removed]">> andalso
               Selftext /= <<>> andalso
               NumComments > MinNoComments ->
            false = maps:get(<<"replies">>, JsonTerm, false),
            false = maps:get(<<"more">>, JsonTerm, false),
            JsonTerm;
        _ ->
            skipped
    end.

populate_comment_db(CommentsFile, SubmissionDb, CommentDb) ->
    {ok, CommentsFd} = file:open(CommentsFile, [read, binary]),
    populate_comment_db(SubmissionDb, CommentDb, CommentsFd, _ParsedLines = 0).

populate_comment_db(SubmissionDb, CommentDb, CommentsFd, ParsedLines) ->
    case file:read_line(CommentsFd) of
        {ok, Line} ->
            case parse_comment(Line, SubmissionDb) of
                skipped ->
                    populate_comment_db(SubmissionDb, CommentDb, CommentsFd, ParsedLines + 1);
                #{<<"parent_id">> := ParentId} = Comment ->
                    ok = dets:insert(CommentDb, {ParentId, Comment}),
                    populate_comment_db(SubmissionDb, CommentDb, CommentsFd, ParsedLines + 1)
            end;
        eof ->
            NoParentIds = dets:info(CommentDb, no_keys),
            NoComments = dets:info(CommentDb, no_objects),
            dets:sync(CommentDb),
            file:close(CommentsFd),
            {NoParentIds, NoComments, ParsedLines}
    end.

parse_comment(Line, SubmissionDb) ->
    case json:decode(Line) of
        #{<<"link_id">> := LinkId} = Comment ->
            case dets:member(SubmissionDb, LinkId) of
                true ->
                    Comment;
                false ->
                    skipped
            end;
        _ ->
            skipped
    end.

insert_subreddit(SubmissionDb, CommentDb) ->
    ok = create_all_users(SubmissionDb, CommentDb),
    dets:traverse(
      SubmissionDb,
      fun({Id, #{<<"author">> := AuthorUsername,
                 <<"created_utc">> := Created,
                 <<"id">> := Id,
                 <<"selftext">> := Selftext,
                 <<"title">> := Title} = Submission}) ->
              io:format("** Inserting submission ~p~n", [Id]),
              Attachments = create_attachments(Submission),
              AuthorId = get_author_id(AuthorUsername),
              Post = #post{id = Id,
                           title = Title,
                           body = Selftext,
                           author = AuthorId,
                           created = patch_created(Created),
                           attachments = Attachments},
              {ok, _} = db_serv:create_post(Post),
              ok = insert_comments(CommentDb, Id),
              continue
      end).

create_attachments(#{<<"is_gallery">> := true, <<"media_metadata">> := MediaMetadata}) ->
    maps:fold(
      fun(ImageHash, #{<<"m">> := ContentType}, Acc) ->
              Extension = apptools_mime:extension(ContentType),
              Filename = io_lib:format("~s.~s", [ImageHash, Extension]),
              FilePath = filename:join(?SUBREDDIT, Filename),
              case filelib:is_regular(FilePath) of
                  true ->
                      io:format("** File ~s already downloaded~n", [FilePath]);
                  false ->
                      Url = io_lib:format("https://i.redd.it/~s.~s", [ImageHash, Extension]),
                      Command = io_lib:format("curl -sq ~s -o ~s", [Url, FilePath]),
                      io:format("** Downloading ~s~n", [Url]),
                      "" = os:cmd(Command)
              end,
              TmpFilePath = filename:join(?BESPOKE_TMP_PATH, Filename),
              {ok, _} = file:copy(FilePath, TmpFilePath),
              [{?l2b(Filename), ContentType}|Acc]
      end, [], MediaMetadata);
create_attachments(#{<<"is_video">> := true,
                     <<"media">> := #{<<"reddit_video">> := #{<<"fallback_url">>:= Url}}}) ->
    AbsPath = filename:basename(Url),
    [Filename|_] = string:lexemes(AbsPath, "?"),
    FilePath = filename:join(?SUBREDDIT, Filename),
    case filelib:is_regular(FilePath) of
        true ->
            io:format("** File ~s already downloaded~n", [FilePath]);
        false ->
            Command = io_lib:format("curl -sq ~s -o ~s", [Url, FilePath]),
            io:format("** Downloading ~s~n", [Url]),
            "" = os:cmd(Command)
    end,
    TmpFilePath = filename:join(?BESPOKE_TMP_PATH, Filename),
    {ok, _} = file:copy(FilePath, TmpFilePath),
    {ok, ContentType} = apptools_mime:mime_type(Filename),
    [{Filename, ContentType}];
create_attachments(_) ->
    [].

create_all_users(SubmissionDb, CommentDb) ->
    _ = dets:traverse(
          SubmissionDb, fun({Id, #{<<"author">> := AuthorUsername}}) ->
                                io:format("** Inserting author ~s~n", [AuthorUsername]),
                                ok = create_author_id(AuthorUsername),
                                ok = crete_all_comment_users(CommentDb, Id),
                                continue
                        end),
    ok.

crete_all_comment_users(CommentDb, ParentId) ->
    Comments = dets:lookup(CommentDb, ParentId),
    lists:foreach(fun({_, #{<<"author">> := AuthorUsername, <<"id">> := Id}}) ->
                          ok = create_author_id(AuthorUsername),
                          crete_all_comment_users(CommentDb, Id)
                  end, Comments).

%% The reddit json api returns the created_utc field as a list!
patch_created(Created) when is_binary(Created) ->
    ?b2i(Created);
patch_created(Created) when is_float(Created) ->
    trunc(Created);
patch_created(Created) when is_integer(Created) ->
    Created.

insert_comments(CommentDb, ParentId) ->
    Comments = dets:lookup(CommentDb, ParentId),
    lists:foreach(fun({_, #{<<"author">> := AuthorUsername,
                            <<"body">> := Body,
                            <<"created_utc">> := Created,
                            <<"id">> := Id,
                            <<"link_id">> := <<_:3/binary, LinkId/binary>>}}) ->
                          AuthorId = get_author_id(AuthorUsername),
                          Post = #post{id = Id,
                                       parent_post_id = ParentId,
                                       top_post_id = LinkId,
                                       body = Body,
                                       author = AuthorId,
                                       created = patch_created(Created)},
                          io:format("** Inserting comment ~p~n", [Id]),
                          {ok, _} = db_serv:create_post(Post),
                          insert_comments(CommentDb, Id)
                  end, Comments).

%%
%% Exported: dump_subreddit_db
%%

dump_subreddit_db() ->
    SubmissionsDbFile = filename:join(?SUBREDDIT, "submissions.db"),
    CommentsDbFile = filename:join(?SUBREDDIT, "comments.db"),
    {SubmissionDb, CommentDb} = open_dbs(SubmissionsDbFile, CommentsDbFile),
    SubmissionsDumpFile = filename:join(?SUBREDDIT, "submissions.dump"),
    Submissions = dets:foldl(
                    fun({_Id, JsonTerm}, Acc) ->
                            [JsonTerm|Acc]
                    end, [], SubmissionDb),
    ok = file:write_file(SubmissionsDumpFile,
                         ?l2b(io_lib:format("~p", [Submissions]))),
    CommentsDumpFile = filename:join(?SUBREDDIT, "comments.dump"),
    Comments = dets:foldl(fun({_ParentId, JsonTerm}, Acc) ->
                                  [JsonTerm|Acc]
                          end, [], CommentDb),
    ok = file:write_file(CommentsDumpFile, ?l2b(io_lib:format("~p", [Comments]))),
    close_dbs(SubmissionDb, CommentDb).

%%
%% Exported: purge_subreddit_db
%%

purge_subreddit_db() ->
    SubmissionsDbFile = filename:join(?SUBREDDIT, "submissions.db"),
    _ = file:delete(SubmissionsDbFile),
    CommentsDbFile = filename:join(?SUBREDDIT, "comments.db"),
    _ = file:delete(CommentsDbFile),
    ok.

%%
%% Utilities
%%

get_author_id(Username) ->
    {ok, User} = db_user_serv:get_user_from_username(Username),
    User#user.id.

create_author_id([]) ->
    ok;
create_author_id([Username|Rest]) ->
    ok = create_author_id(Username),
    create_author_id(Rest);
create_author_id(Username) when is_binary(Username) ->
    case db_user_serv:get_user_from_username(Username) of
        {ok, _User} ->
            ok;
        {error, _} ->
            {ok, _User} = db_user_serv:insert_user(Username),
            ok
    end.
