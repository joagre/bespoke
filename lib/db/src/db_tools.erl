-module(db_tools).
-export([create_subreddit_db/0, purge_subreddit_db/0, dump_subreddit_db/0,
         create_dummy_db/0]).

-include("../include/db.hrl").
-include("../../apptools/include/log.hrl").
-include("../../apptools/include/shorthand.hrl").

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
          "/media/jocke/EXTERNSL/reddit/subreddits23/sweden_submissions",
          "/media/jocke/EXTERNSL/reddit/subreddits23/sweden_comments",
          _NoSubmissions = 10,
          _MinNoComments = 25),
    [] = insert_subreddit(SubmissionDb, CommentDb),
    close_dbs(SubmissionDb, CommentDb).

get_cached_subreddit(SubmissionsFile, CommentsFile, NoSubmissions,
                     MinNoComments) ->
    SubmissionsDbFile =
        filename:join(code:priv_dir(webapp), "submissions.db"),
    CommentsDbFile =
        filename:join(code:priv_dir(webapp), "comments.db"),
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
                    {SubmissionDb, CommentDb} =
                        open_dbs(SubmissionsDbFile, CommentsDbFile),
                    io:format("** Creating new comment database~n"),
                    {NoParentsIds, NoComments, ParsedComments} =
                        populate_comment_db(
                          CommentsFile, SubmissionDb, CommentDb),
                    io:format("** Total number of parent ids = ~p~n",
                              [NoParentsIds]),
                    io:format("** Total number of comments = ~p~n",
                              [NoComments]),
                    io:format("** Parsed ~p comments~n", [ParsedComments]),
                    {SubmissionDb, CommentDb}
            end;
        false ->
            {SubmissionDb, CommentDb} =
                open_dbs(SubmissionsDbFile, CommentsDbFile),
            io:format("** Creating new submission database~n"),
            {NoSubmissions, ParsedSubmissions} =
                populate_submission_db(
                  SubmissionsFile, SubmissionDb, NoSubmissions, MinNoComments),
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
    {ok, SubmissionDb} =
        dets:open_file(submissions, [{file, SubmissionsDbFile}]),
    {ok, CommentDb} =
        dets:open_file(comments, [{file, CommentsDbFile}, {type, bag}]),
    {SubmissionDb, CommentDb}.

close_dbs(SubmissionDb, CommentDb) ->
    dets:close(SubmissionDb),
    dets:close(CommentDb).

populate_submission_db(SubmissionsFile, SubmissionDb, NoSubmissions,
                       MinNoComments) ->
    {ok, SubmissionsFd} = file:open(SubmissionsFile, [read, binary]),
    populate_submission_db(SubmissionDb, NoSubmissions, MinNoComments,
                           SubmissionsFd, _ParsedLines = 0).

populate_submission_db(SubmissionDb, 0, _MinNoComments, SubmissionsFd,
                       ParsedLines) ->
    NoSubmissions = dets:info(SubmissionDb, no_keys),
    dets:sync(SubmissionDb),
    file:close(SubmissionsFd),
    {NoSubmissions, ParsedLines};
populate_submission_db(SubmissionDb, NoSubmissions, MinNoComments,
                       SubmissionsFd, ParsedLines) ->
    case file:read_line(SubmissionsFd) of
        {ok, Line} ->
            case parse_submission(Line, MinNoComments) of
                skipped ->
                    populate_submission_db(SubmissionDb, NoSubmissions,
                                           MinNoComments, SubmissionsFd,
                                           ParsedLines + 1);
                #{<<"id">> := Id} = JsonTerm ->
                    ok = dets:insert(SubmissionDb, {Id, JsonTerm}),
                    populate_submission_db(SubmissionDb, NoSubmissions - 1,
                                           MinNoComments, SubmissionsFd,
                                           ParsedLines + 1)
            end;
        eof ->
            file:close(SubmissionsFd),
            ParsedLines
    end.

parse_submission(Line, MinNoComments) ->
    case json:decode(Line) of
        #{<<"is_self">> := true,
          <<"num_comments">> := NumComments,
          <<"selftext">> := Selftext} = JsonTerm
          when (Selftext /= <<"[deleted]">> andalso
                Selftext /= <<"[removed]">>) andalso
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
                    populate_comment_db(SubmissionDb, CommentDb, CommentsFd,
                                        ParsedLines + 1);
                #{<<"parent_id">> := ParentId} = Comment ->
                    ok = dets:insert(CommentDb, {ParentId, Comment}),
                    populate_comment_db(SubmissionDb, CommentDb, CommentsFd,
                                        ParsedLines + 1)
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
    dets:traverse(
      SubmissionDb, fun({Id, #{<<"author">> := Author,
                               <<"created_utc">> := Created,
                               <<"id">> := Id,
                               <<"selftext">> := Body,
                               <<"title">> := Title}}) ->
                            io:format("** Inserting submission ~p~n", [Id]),
                            Post = #post{id = Id,
                                         title = Title,
                                         body = Body,
                                         author = Author,
                                         created = Created},
                            {ok, _} = db_serv:insert_post(
                                        patch_post(Post)),
                            ok = insert_comments(CommentDb, Id),
                            continue
                    end).

%% The reddit json api returns the created_utc field as a list!
patch_post(#post{created = Created} = Post) when is_binary(Created) ->
    Post#post{created = ?b2i(Created)};
patch_post(Post) ->
    Post.

insert_comments(CommentDb, ParentId) ->
    Comments = dets:lookup(CommentDb, ParentId),
    lists:foreach(fun({_, #{<<"author">> := Author,
                            <<"body">> := Body,
                            <<"created_utc">> := Created,
                            <<"id">> := Id,
                            <<"link_id">> := <<_:3/binary, LinkId/binary>>}}) ->
                          Post = #post{id = Id,
                                       parent_post_id = ParentId,
                                       top_post_id = LinkId,
                                       body = Body,
                                       author = Author,
                                       created = Created},
                          io:format("** Inserting comment ~p~n", [Id]),
                          {ok, _} = db_serv:insert_post(
                                      patch_post(Post)),
                          insert_comments(CommentDb, Id)
                  end, Comments).

%%
%% Exported: dump_subreddit_db
%%

dump_subreddit_db() ->
    SubmissionsDbFile = filename:join(code:priv_dir(webapp), "submissions.db"),
    CommentsDbFile = filename:join(code:priv_dir(webapp), "comments.db"),
    {SubmissionDb, CommentDb} = open_dbs(SubmissionsDbFile, CommentsDbFile),
    SubmissionsDumpFile =
        filename:join(code:priv_dir(webapp), "submissions.dump"),
    Submissions = dets:foldl(
                    fun({_Id, JsonTerm}, Acc) ->
                            [JsonTerm|Acc]
                    end, [], SubmissionDb),
    ok = file:write_file(SubmissionsDumpFile,
                         ?l2b(io_lib:format("~p", [Submissions]))),
    CommentsDumpFile = filename:join(code:priv_dir(webapp), "comments.dump"),
    Comments = dets:foldl(
                 fun({_ParentId, JsonTerm}, Acc) ->
                         [JsonTerm|Acc]
                 end, [], CommentDb),
    ok = file:write_file(CommentsDumpFile,
                         ?l2b(io_lib:format("~p", [Comments]))),
    close_dbs(SubmissionDb, CommentDb).

%%
%% Exported: purge_subreddit_db
%%

purge_subreddit_db() ->
    SubmissionsDbFile = filename:join(code:priv_dir(webapp), "submissions.db"),
    _ = file:delete(SubmissionsDbFile),
    CommentsDbFile = filename:join(code:priv_dir(webapp), "comments.db"),
    _ = file:delete(CommentsDbFile),
    ok.

%%
%% Exported: create_dummy_db
%%

create_dummy_db() ->
    %% Add two top posts
    TopPost1 = #post{title = <<"Var har sillen tagit vagen?">>,
                     body = <<"body">>,
                     author = <<"ginko4711">>},
    {ok, InsertedTopPost1} = db_serv:insert_post(TopPost1),
    TopPost2 = #post{title = <<"Republik nu!">>,
                     body = <<"body">>,
                     author = <<"zappeU">>},
    {ok, _InsertedTopPost2} = db_serv:insert_post(TopPost2),
    %% Add two replies
    ReplyPost1 =
        #post{parent_post_id = InsertedTopPost1#post.id,
              top_post_id = InsertedTopPost1#post.id,
              body = <<"reply1">>,
              author = <<"snuvan">>},
    {ok, InsertedReplyPost1} = db_serv:insert_post(ReplyPost1),
    ReplyPost2 =
        #post{parent_post_id = InsertedTopPost1#post.id,
              top_post_id = InsertedTopPost1#post.id,
              body = <<"reply2">>,
              author = <<"harald">>},
    {ok, _InsertedReplyPost2} = db_serv:insert_post(ReplyPost2),
    %% Add two replies to the first reply
    ReplyPost11 =
        #post{parent_post_id = InsertedReplyPost1#post.id,
              top_post_id = InsertedTopPost1#post.id,
              body = <<"reply11">>,
              author = <<"sminkor">>},
    {ok, _InsertedReplyPost11} = db_serv:insert_post(ReplyPost11),
    ReplyPost12 =
        #post{parent_post_id = InsertedReplyPost1#post.id,
              top_post_id = InsertedTopPost1#post.id,
              body = <<"reply12">>,
              author = <<"honan">>},
    {ok, _InsertedReplyPost12} = db_serv:insert_post(ReplyPost12),
    ok = db_serv:sync().