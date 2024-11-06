-module(db_tools).
-export([create_subreddit_db/0, purge_subreddit_db/0, create_dummy_db/0]).

-include("db.hrl").
-include("../../apptools/include/log.hrl").

%%
%% Exported: dump_subreddit
%%

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
    insert_subreddit(SubmissionDb, CommentDb).

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
                    io:format("** Total number of parentd ids = ~p~n",
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
            io:format("** Total number of parentd ids = ~p~n", [NoParentsIds]),
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
                #{<<"parent_id">> :=
                      <<_:3/binary, ParentId/binary>>} = Comment ->
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
        #{<<"link_id">> := <<"t3_", LinkId/binary>>} = Comment ->
            case dets:member(SubmissionDb, LinkId) of
                true ->
                    Comment;
                false ->
                    skipped
            end;
        _ ->
            skipped
    end.

insert_subreddit(_Submissions, _Comments) ->
    ok.

%% save below awhile as inspiration



%% parse_submission(Line, MinNoComments) ->
%%     case json:decode(Line) of
%%         #{<<"author">> := Author,
%%           <<"created_utc">> := Created,
%%           <<"id">> := Id,
%%           <<"is_self">> := true,
%%           <<"num_comments">> := NumComments,
%%           <<"selftext">> := Selftext,
%%           <<"title">> := Title} = JsonTerm
%%           when (Selftext /= <<"[deleted]">> andalso
%%                 Selftext /= <<"[removed]">>) andalso
%%                NumComments > MinComments ->
%%             false = maps:get(<<"replies">>, JsonTerm, false),
%%             false = maps:get(<<"more">>, JsonTerm, false),
%%             %%io:format("++++++++++++++++++\n"),
%%             %%io:format("Line: ~p~n", [Line]),
%%             #message{id = Id,
%%                      title = Title,
%%                      body = Selftext,
%%                      author = Author,
%%                      created = Created,
%%                      reply_count = NumComments};
%%         _ ->
%%             skipped
%%     end.








%% create_reddit_db(SubmissionsFile, CommentsFile, Limit) ->
%%     %% Parse submissions
%%     {ok, Submissions} = file:open(SubmissionsFile, [read, binary]),
%%     {SubmissionsParsed, ParsedSubmissions} =
%%         parse_submissions(Submissions, Limit),
%%     file:close(Submissions),
%%     {ok, Comments} = file:open(CommentsFile, [read, binary]),
%%     %% Parse comments
%%     {CommentsParsed, ParsedComments} =
%%         parse_comments(Comments, ParsedSubmissions),
%%     file:close(Comments),
%%     {SubmissionsParsed, CommentsParsed, ParsedSubmissions, ParsedComments}.

%% parse_submissions(Submissions, Limit) ->
%%     parse_submissions(Submissions, Limit, 0, #{}).

%% parse_submissions(_Submissions, 0, LinesParsed, Acc) ->
%%     {LinesParsed, Acc};
%% parse_submissions(Submissions, Limit, LinesParsed, Acc) ->
%%     case file:read_line(Submissions) of
%%         {ok, Line} ->
%%             case parse_submission(Line, 15) of
%%                 skipped ->
%%                     parse_submissions(Submissions, Limit, LinesParsed + 1, Acc);
%%                 Message ->
%%                     parse_submissions(Submissions, Limit - 1, LinesParsed + 1,
%%                                       maps:put(Message#message.id, Message, Acc))
%%             end;
%%         eof ->
%%             {LinesParsed, Acc}
%%     end.

%% parse_submission(Line, MinComments) ->
%%     case json:decode(Line) of
%%         #{<<"author">> := Author,
%%           <<"created_utc">> := Created,
%%           <<"id">> := Id,
%%           <<"is_self">> := true,
%%           <<"num_comments">> := NumComments,
%%           <<"selftext">> := Selftext,
%%           <<"title">> := Title} = JsonTerm
%%           when (Selftext /= <<"[deleted]">> andalso
%%                 Selftext /= <<"[removed]">>) andalso
%%                NumComments > MinComments ->
%%             false = maps:get(<<"replies">>, JsonTerm, false),
%%             false = maps:get(<<"more">>, JsonTerm, false),
%%             %%io:format("++++++++++++++++++\n"),
%%             %%io:format("Line: ~p~n", [Line]),
%%             #message{id = Id,
%%                      title = Title,
%%                      body = Selftext,
%%                      author = Author,
%%                      created = Created,
%%                      reply_count = NumComments};
%%         _ ->
%%             skipped
%%     end.

%% parse_comments(Comments, ParsedSubmissions) ->
%%     parse_comments(Comments, ParsedSubmissions, 0, #{}).

%% parse_comments(Comments, ParsedSubmissions, LinesParsed, Acc) ->
%%     case file:read_line(Comments) of
%%         {ok, Line} ->
%%             case parse_comment(Line, ParsedSubmissions) of
%%                 skipped ->
%%                     parse_comments(Comments, ParsedSubmissions, LinesParsed + 1,
%%                                    Acc);
%%                 {comment, Id} ->
%%                     parse_comments(Comments, ParsedSubmissions, LinesParsed + 1,
%%                                    maps:put(Id, comment, Acc))
%%             end;
%%         eof ->
%%             {LinesParsed, Acc}
%%     end.

%% parse_comment(Line, ParsedSubmissions) ->
%%     case json:decode(Line) of
%%         #{<<"id">> := Id,
%%           <<"link_id">> := <<"t3_", LinkId/binary>>} ->
%%             case maps:is_key(LinkId, ParsedSubmissions) of
%%                 false ->
%%                     skipped;
%%                 true ->
%%                     {comment, Id}
%%             end;
%%         _ ->
%%             skipped
%%     end.

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
    %% Add two root messages
    RootMessage1 = #message{title = "Var har sillen tagit vagen?",
                            body = "body",
                            author = "ginko4711"},
    {ok, InsertedRootMessage1} = db_serv:insert_message(RootMessage1),
    RootMessage2 = #message{title = "Republik nu!",
                            body = "body",
                            author = "zappeU"},
    {ok, _InsertedRootMessage2} = db_serv:insert_message(RootMessage2),
    %% Add two replies
    ReplyMessage1 =
        #message{parent_message_id = InsertedRootMessage1#message.id,
                 root_message_id = InsertedRootMessage1#message.id,
                 body = "reply1",
                 author = "snuvan"},
    {ok, InsertedReplyMessage1} = db_serv:insert_message(ReplyMessage1),
    ReplyMessage2 =
        #message{parent_message_id = InsertedRootMessage1#message.id,
                 root_message_id = InsertedRootMessage1#message.id,
                 body = "reply2",
                 author = "harald"},
    {ok, _InsertedReplyMessage2} = db_serv:insert_message(ReplyMessage2),
    %% Add two replies to the first reply
    ReplyMessage11 =
        #message{parent_message_id = InsertedReplyMessage1#message.id,
                 root_message_id = InsertedRootMessage1#message.id,
                 body = "reply11",
                 author = "sminkor"},
    {ok, _InsertedReplyMessage11} = db_serv:insert_message(ReplyMessage11),
    ReplyMessage12 =
        #message{parent_message_id = InsertedReplyMessage1#message.id,
                 root_message_id = InsertedRootMessage1#message.id,
                 body = "reply12",
                 author = "honan"},
    {ok, _InsertedReplyMessage12} = db_serv:insert_message(ReplyMessage12),
    ok = db_serv:sync().
