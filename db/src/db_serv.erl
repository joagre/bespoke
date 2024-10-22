-module(db_serv).

-export([start_link/0, stop/0]).
-export([list_topics/0, lookup_topic/1, insert_topic/1]).
-export([lookup_reply/1, insert_reply/1]).
-export([message_handler/1]).

-export_type([topic_id/0, title/0, body/0, author/0,
              seconds_from_epoch/0, reply_id/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("apptools/include/serv.hrl").
-include("db.hrl").

-type topic_id() :: integer().
-type title() :: string().
-type body() :: string().
-type author() :: string().
-type seconds_from_epoch() :: integer().
-type reply_id() :: integer().

-record(state, {
                parent :: pid(),
                next_topic_id = 0 :: topic_id(),
                next_reply_id = 0 :: reply_id()
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
%% Exported: list_topics
%%

-spec list_topics() -> [#topic{}].

list_topics() ->
    serv:call(?MODULE, list_topics).

%%
%% Exported: lookup_topic
%%

-spec lookup_topic(topic_id()) -> [#topic{}].

lookup_topic(TopicId) ->
    serv:call(?MODULE, {lookup_topic, TopicId}).

%%
%% Exported: insert_topic
%%

-spec insert_topic(#topic{}) -> #topic{}.

insert_topic(Topic) ->
    serv:call(?MODULE, {insert_topic, Topic}).

%%
%% Exported: lookup_reply
%%

-spec lookup_reply(reply_id()) -> [#reply{}].

lookup_reply(ReplyId) ->
    serv:call(?MODULE, {lookup_reply, ReplyId}).

%%
%% Exported: insert_reply
%%

-spec insert_reply(#reply{}) ->
          {ok, #reply{}} | {error, no_such_topic}.

insert_reply(Reply) ->
    serv:call(?MODULE, {insert_reply, Reply}).

%%
%% Server
%%

init(Parent) ->
    {ok, topics} =
        dets:open_file(topics,
                       [{file, filename:join(code:priv_dir(db), "topics.db")},
                        {keypos, #topic.id}]),
    {ok, replies} =
        dets:open_file(replies,
                       [{file, filename:join(code:priv_dir(db), "replies.db")},
                        {keypos, #reply.id}]),
    ?log_info("Database server has been started"),
    {ok, #state{parent = Parent}}.

message_handler(S) ->
    receive
        {call, From, stop = Call} ->
            ?log_debug(#{call => Call}),
            ok = dets:close(topics),
            {reply, From, ok};
        {call, From, list_topics = Call} ->
            ?log_debug(#{call => Call}),
            AllTopics = dets:foldl(fun(Topic, Acc) ->
                                           [Topic|Acc] end,
                                   [], topics),
            SortedTopics =
                lists:sort(fun(TopicA, TopicB) ->
                                   TopicA#topic.created =< TopicB#topic.created
                           end, AllTopics),
            {reply, From, SortedTopics};
        {call, From, {lookup_topic, TopicId} = Call} ->
            ?log_debug(#{call => Call}),
            {reply, From, dets:lookup(topics, TopicId)};
        {call, From, {insert_topic, Topic} = Call} ->
            ?log_debug(#{call => Call}),
            NextTopicId = S#state.next_topic_id,
            UpdatedTopic = Topic#topic{id = NextTopicId,
                                       created = seconds_from_epoch()},
            ok = dets:insert(topics, UpdatedTopic),
            {reply, From, UpdatedTopic, S#state{next_topic_id = NextTopicId + 1}};
        {call, From, {lookup_reply, ReplyId} = Call} ->
            ?log_debug(#{call => Call}),
            {reply, From, dets:lookup(replies, ReplyId)};
        {call, From, {insert_reply, Reply} = Call} ->
            ?log_debug(#{call => Call}),
            case add_reply(S#state.next_reply_id, Reply) of
                {ok, NextReplyId, AddedReply} ->
                    {reply, From, {ok, AddedReply},
                     S#state{next_reply_id = NextReplyId}};
                {error, Reason} ->
                    {reply, From, {error, Reason}}
            end;
        {'EXIT', Pid, Reason} when S#state.parent == Pid ->
            exit(Reason);
        {system, From, Request} ->
            ?log_debug(#{system => {From, Request}}),
            {system, From, Request};
        UnknownMessage ->
            ?log_error(#{unknown_message => UnknownMessage}),
            noreply
    end.

%%
%% Insert reply
%%

add_reply(ReplyId, Reply) ->
    case dets:lookup(topics, Reply#reply.topic_id) of
        [Topic] ->
            case valid_reply(Reply#reply.reply_id) of
                true ->
                    UpdatedReply = Reply#reply{id = ReplyId,
                                               created = seconds_from_epoch()},
                    ok = dets:insert(replies, UpdatedReply),
                    Replies = Topic#topic.replies ++ [ReplyId],
                    ok = dets:insert(topics, Topic#topic{replies = Replies}),
                    {ok, ReplyId + 1, UpdatedReply};
                false ->
                    {error, no_such_reply}
            end;
        [] ->
            {error, no_such_topic}
    end.

valid_reply(not_set) ->
    true;
valid_reply(ReplyId) ->
    case dets:lookup(replies, ReplyId) of
        [_Reply] ->
            true;
        [] ->
            false
    end.

%%
%% Utilities
%%

seconds_from_epoch() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).
