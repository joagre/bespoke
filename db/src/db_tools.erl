-module(db_tools).
-export([dump_subreddit/0, dump_subreddit/3, create_dummy_db/0]).

-include("db.hrl").
-include("../../apptools/include/log.hrl").

%%
%% Exported: dump_subreddit
%%

dump_subreddit() ->
    dump_subreddit("/media/jocke/EXTERNSL/reddit/subreddits23/sweden_submissions",
                   "/media/jocke/EXTERNSL/reddit/subreddits23/sweden_comments",
                   10).

dump_subreddit(SubmissionsFile, CommentsFile, Limit) ->
    {ok, Submissions} = file:open(SubmissionsFile, [read, binary]),
    {ok, Comments} = file:open(CommentsFile, [read, binary]),
    ParsedSubmissions = parse_subreddit(Submissions, Comments, Limit),
    file:close(Submissions),
    file:close(Comments),
    ParsedSubmissions.

parse_subreddit(Submissions, Comments, Limit) ->
    parse_subreddit(Submissions, Comments, Limit, 0, []).

parse_subreddit(_Submissions, _Comments, 0, LinesParsed, Acc) ->
    {LinesParsed, Acc};
parse_subreddit(Submissions, Comments, Limit, LinesParsed, Acc) ->
    case file:read_line(Submissions) of
        {ok, SubmissionLine} ->
            case parse_submission(SubmissionLine) of
                skipped ->
                    parse_subreddit(Submissions, Comments, Limit,
                                    LinesParsed + 1, Acc);
                Message ->
                    parse_subreddit(Submissions, Comments, Limit - 1,
                                    LinesParsed + 1, [Message|Acc])
            end;
        eof ->
            {LinesParsed, Acc}
    end.

parse_submission(Line) ->
    case json:decode(Line) of
        #{<<"is_self">> := true, <<"selftext">> := Selftext}
          when Selftext /= <<"[deleted]">> andalso Selftext /= <<"[removed]">> ->
            #message{body = binary_to_list(Selftext)};
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
