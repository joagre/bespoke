-module(db_tools).
-export([create_reddit_db/0, create_dummy_db/0]).

-include("db.hrl").
-include("../../apptools/include/log.hrl").

%%
%% Exported: dump_subreddit
%%

create_reddit_db() ->
    create_reddit_db(
      "/media/jocke/EXTERNSL/reddit/subreddits23/sweden_submissions",
      "/media/jocke/EXTERNSL/reddit/subreddits23/sweden_comments",
      10).

create_reddit_db(SubmissionsFile, CommentsFile, Limit) ->
    %% Parse submissions
    {ok, Submissions} = file:open(SubmissionsFile, [read, binary]),
    {SubmissionsParsed, ParsedSubmissions} =
        parse_submissions(Submissions, Limit),
    file:close(Submissions),
    {ok, Comments} = file:open(CommentsFile, [read, binary]),
    %% Parse comments
    {CommentsParsed, ParsedComments} =
        parse_comments(Comments, ParsedSubmissions),
    file:close(Comments),
    {SubmissionsParsed, CommentsParsed, ParsedSubmissions, ParsedComments}.

parse_submissions(Submissions, Limit) ->
    parse_submissions(Submissions, Limit, 0, #{}).

parse_submissions(_Submissions, 0, LinesParsed, Acc) ->
    {LinesParsed, Acc};
parse_submissions(Submissions, Limit, LinesParsed, Acc) ->
    case file:read_line(Submissions) of
        {ok, Line} ->
            case parse_submission(Line, 15) of
                skipped ->
                    parse_submissions(Submissions, Limit, LinesParsed + 1, Acc);
                Message ->
                    parse_submissions(Submissions, Limit - 1, LinesParsed + 1,
                                      maps:put(Message#message.id, Message, Acc))
            end;
        eof ->
            {LinesParsed, Acc}
    end.

parse_submission(Line, MinComments) ->
    case json:decode(Line) of
        #{<<"author">> := Author,
          <<"created_utc">> := Created,
          <<"id">> := Id,
          <<"is_self">> := true,
          <<"num_comments">> := NumComments,
          <<"selftext">> := Selftext,
          <<"title">> := Title} = JsonTerm
          when (Selftext /= <<"[deleted]">> andalso
                Selftext /= <<"[removed]">>) andalso
               NumComments > MinComments ->
            false = maps:get(<<"replies">>, JsonTerm, false),
            false = maps:get(<<"more">>, JsonTerm, false),
            %%io:format("++++++++++++++++++\n"),
            %%io:format("Line: ~p~n", [Line]),
            #message{id = Id,
                     title = Title,
                     body = Selftext,
                     author = Author,
                     created = Created,
                     reply_count = NumComments};
        _ ->
            skipped
    end.

parse_comments(Comments, ParsedSubmissions) ->
    parse_comments(Comments, ParsedSubmissions, 0, #{}).

parse_comments(Comments, ParsedSubmissions, LinesParsed, Acc) ->
    case file:read_line(Comments) of
        {ok, Line} ->
            case parse_comment(Line, ParsedSubmissions) of
                skipped ->
                    parse_comments(Comments, ParsedSubmissions, LinesParsed + 1,
                                   Acc);
                {comment, Id} ->
                    parse_comments(Comments, ParsedSubmissions, LinesParsed + 1,
                                   maps:put(Id, comment, Acc))
            end;
        eof ->
            {LinesParsed, Acc}
    end.

parse_comment(Line, ParsedSubmissions) ->
    case json:decode(Line) of
        #{<<"id">> := Id,
          <<"link_id">> := <<"t3_", LinkId/binary>>} ->
            case maps:is_key(LinkId, ParsedSubmissions) of
                false ->
                    skipped;
                true ->
                    {comment, Id}
            end;
        _ ->
            skipped
    end.

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
