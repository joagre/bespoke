-module(test_db).
-export([start/0]).

-include("../src/db.hrl").
-include("../../apptools/include/log.hrl").

start() ->
    %% No top posts inserted yet
    [] = db_serv:list_top_posts(),
    %% Add top post
    TopPost1 = #post{title = "title1",
                     body = "body1",
                     author = "author1"},
    {ok, InsertedTopPost1} = db_serv:insert_post(TopPost1),
    %% Verify post
    [#post{id = <<"0">>,
           title = "title1",
           parent_post_id = not_set,
           top_post_id = not_set,
           body = "body1",
           author = "author1",
           created = Created,
           reply_count = 0,
           replies = []}] =
        db_serv:lookup_posts([InsertedTopPost1#post.id]),
    true = is_integer(Created),
    %% Add reply post to top post
    ReplyPost1 =
        #post{parent_post_id = InsertedTopPost1#post.id,
              top_post_id = InsertedTopPost1#post.id,
              body = "reply1",
              author = "author2"},
    {ok, InsertedReplyPost1} = db_serv:insert_post(ReplyPost1),
    %% Add reply post to reply post
    ReplyPost2 =
        #post{parent_post_id = InsertedReplyPost1#post.id,
              top_post_id = InsertedTopPost1#post.id,
              body = "reply2",
              author = "author3"},
    {ok, InsertedReplyPost2} = db_serv:insert_post(ReplyPost2),
    %% Verify top post
    ReplyPostId1 = InsertedReplyPost1#post.id,
    [#post{replies = [ReplyPostId1]}] =
        db_serv:lookup_posts([InsertedTopPost1#post.id]),
    %% Verify reply posts
    ReplyPostId2 = InsertedReplyPost2#post.id,
    [#post{replies = [ReplyPostId2]}] =
        db_serv:lookup_posts([InsertedReplyPost1#post.id]),
    [#post{replies = []}] =
        db_serv:lookup_posts([InsertedReplyPost2#post.id]),
    %% Verify reply counts
    [#post{reply_count = 2}] =
        db_serv:lookup_posts([InsertedTopPost1#post.id]),
    [#post{reply_count = 1}] =
        db_serv:lookup_posts([InsertedReplyPost1#post.id]),
    [#post{reply_count = 0}] =
        db_serv:lookup_posts([InsertedReplyPost2#post.id]).
