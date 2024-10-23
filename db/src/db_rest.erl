-module(db_rest).
-export([start_link/0]).
-export([request_handler/4]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("rester/include/rester.hrl").
-include_lib("rester/include/rester_http.hrl").
-include("db.hrl").

%%
%% Exported: start_link
%%

start_link() ->
    Options =
	[{request_handler, {?MODULE, request_handler, []}},
	 {nodelay, true},
	 {reuseaddr, true}],
    ?log_info("Database REST API has been started"),
    rester_http_server:start_link(8080, Options).

%%
%% Exported: request_handler
%%

request_handler(Socket, Request, Body, Options) ->
    ?log_info("Request = ~s, Headers = ~s, Body = ~p",
              [rester_http:format_request(Request),
               rester_http:format_hdr(Request#http_request.headers),
               Body]),
    try request_handler_(Socket, Request, Body, Options) of
	Result ->
            Result
    catch
	_Class:Reason:StackTrace ->
	    ?log_error("request_handler crashed: ~p\n~p\n",
                       [Reason, StackTrace]),
	    erlang:error(Reason)
    end.

request_handler_(Socket, Request, Body, Options) ->
    case Request#http_request.method of
	'GET' ->
	    http_get(Socket, Request, Body, Options);
	'POST' ->
	    http_post(Socket, Request, Body, Options);
	_ ->
	    rest_util:response(Socket, Request, {error, not_allowed})
    end.

%% Handle GET request
%% - [vi]/index.htm[l]
%% - /versions                        return an json array of supported versions

http_get(Socket, Request, Body, Options) ->
    Url = Request#http_request.uri,
    case string:tokens(Url#url.path, "/") of
	["versions"] ->
	    Object = jsone:encode([v1]),
	    rester_http_server:response_r(Socket, Request, 200, "OK", Object,
                                          [{content_type,"application/json"}]);
	["v1"|Tokens] ->
	    http_get(Socket, Request, Options, Url, Tokens, Body, v1);
	Tokens ->
	    http_get(Socket, Request, Options, Url, Tokens,  Body, v1)
    end.

http_get(Socket, Request, _Options, Url, Tokens, _Body, v1) ->
    case Tokens of
        ["list_topics"] ->
            Topics = db_serv:list_topics(),
            JsonTerm = lists:map(fun(Topic) ->
                                         topic_to_json_term(Topic)
                                 end, Topics),
            rest_util:response(Socket, Request, {ok, {format, JsonTerm}});
        ["lookup_topic", Id] ->
            case string:to_integer(Id) of
                {IdInteger, ""} ->
                    case db_serv:lookup_topic(IdInteger) of
                        [] ->
                            rest_util:response(Socket, Request,
                                               {error, not_found});
                        [Topic]->
                            JsonTerm = topic_to_json_term(Topic),
                            rest_util:response(Socket, Request,
                                               {ok, {format, JsonTerm}})
                    end;
                _ ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "topic-id must be an integer"})
            end;
        ["lookup_reply", Id] ->
            case string:to_integer(Id) of
                {IdInteger, ""} ->
                    case db_serv:lookup_reply(IdInteger) of
                        [] ->
                            rest_util:response(Socket, Request,
                                               {error, not_found});
                        [Reply]->
                            JsonTerm = reply_to_json_term(Reply),
                            rest_util:response(Socket, Request,
                                               {ok, {format, JsonTerm}})
                    end;
                _ ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "reply-id must be an integer"})
            end;
        %% Try to act as a static web server
	Tokens ->
            UriPath =
                case Tokens of
                    [] ->
                        "/index.html";
                    _ ->
                        Url#url.path
                end,
            AbsFilename =
                filename:join(
                  [filename:absname(code:priv_dir(webapp)), "docroot",
                   tl(UriPath)]),
            case filelib:is_regular(AbsFilename) of
                true ->
                    rester_http_server:response_r(
                      Socket, Request, 200, "OK", {file, AbsFilename},
                      [{content_type, {url, UriPath}}]);
                false ->
                    ?log_info("~p not found", [Tokens]),
                    rest_util:response(Socket, Request, {error, not_found})
            end
    end.

%% General POST request uri:
%% - [/vi]/item
%%
http_post(Socket, Request, Body, Options) ->
    Url = Request#http_request.uri,
    case string:tokens(Url#url.path, "/") of
	["v1" | Tokens] ->
	    http_post(Socket, Request, Options, Url, Tokens, Body, v1);
	Tokens ->
	    http_post(Socket, Request, Options, Url, Tokens, Body, v1)
    end.

http_post(Socket, Request, _Options, _Url, Tokens, Body, v1) ->
    case Tokens of
        ["insert_topic"] ->
            case rest_util:parse_body(
                   Request, Body,
                   [{jsone_options, [{object_format, proplist}]}]) of
                {error, _Reason} ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"});
                RequestJsonTerm ->
                    case json_term_to_topic(RequestJsonTerm) of
                        {ok, Topic} ->
                            InsertedTopic = db_serv:insert_topic(Topic),
                            ResponseJsonTerm =
                                topic_to_json_term(InsertedTopic),
                            rest_util:response(
                              Socket, Request,
                              {ok, {format, ResponseJsonTerm}});
                        invalid ->
                            rest_util:response(
                              Socket, Request,
                              {error, bad_request, "Invalid topic"})
                    end
            end;
        ["insert_reply"] ->
            case rest_util:parse_body(
                   Request, Body,
                   [{jsone_options, [{object_format, proplist}]}]) of
                {error, _Reason} ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"});
                RequestJsonTerm ->
                    case json_term_to_reply(RequestJsonTerm) of
                        {ok, Reply} ->
                            case db_serv:insert_reply(Reply) of
                                {ok, InsertedReply} ->
                                    ResponseJsonTerm =
                                        reply_to_json_term(InsertedReply),
                                    rest_util:response(
                                      Socket, Request,
                                      {ok, {format, ResponseJsonTerm}});
                                {error, no_such_topic} ->
                                    rest_util:response(
                                      Socket, Request, {error, not_found})
                            end;
                        invalid ->
                            rest_util:response(
                              Socket, Request,
                              {error, bad_request, "Invalid reply"})
                    end
            end;
	_ ->
	    ?log_error("~p not found", [Tokens]),
	    rest_util:response(Socket, Request, {error, not_found})
    end.

%%
%% Utilities
%%

topic_to_json_term(#topic{
                      id = Id,
                      title = Title,
                      body = Body,
                      author = Author,
                      created = Created,
                      replies = Replies}) ->
    [{<<"id">>, Id},
     {<<"title">>, ?l2b(Title)},
     {<<"body">>, ?l2b(Body)},
     {<<"author">>, ?l2b(Author)},
     {<<"created">>, Created},
     {<<"replies">>, Replies}].

reply_to_json_term(#reply{
                      id = Id,
                      topic_id = TopicId,
                      reply_id = ReplyId,
                      body = Body,
                      author = Author,
                      created = Created}) ->
    [{<<"id">>, ?l2b(Id)},
     {<<"topic-id">>, ?l2b(TopicId)},
     {<<"reply-id">>, format_reply_id(ReplyId)},
     {<<"body">>, ?l2b(Body)},
     {<<"author">>, ?l2b(Author)},
     {<<"created">>, Created}].

format_reply_id(not_set) ->
    <<"">>;
format_reply_id(Id) ->
    ?l2b(Id).

json_term_to_topic(JsonTerm) when is_list(JsonTerm) ->
    case lists:keysort(1, JsonTerm) of
        [{<<"author">>, Author},
         {<<"body">>, Body},
         {<<"title">>, Title}] ->
            {ok, #topic{title = ?b2l(Title),
                        body = ?b2l(Body),
                        author = ?b2l(Author)}};
        _ ->
            invalid
    end;
json_term_to_topic(_) ->
    invalid.

json_term_to_reply(JsonTerm) when is_list(JsonTerm) ->
    case lists:keysort(1, JsonTerm) of
        [{<<"author">>, Author},
         {<<"body">>, Body},
         {<<"reply-id">>, ReplyId},
         {<<"topic-id">>, TopicId}] ->
            case string:to_integer(ReplyId) of
                {ReplyIdInteger, ""} ->
                    {ok, #reply{topic_id = TopicId,
                                reply_id = ReplyIdInteger,
                                body = ?b2l(Body),
                                author = ?b2l(Author)}};
                _ ->
                    invalid
            end;
        [{<<"topic-id">>, TopicId},
         {<<"body">>, Body},
         {<<"author">>, Author}] ->
            {ok, #reply{topic_id = TopicId,
                        body = ?b2l(Body),
                        author = ?b2l(Author)}};
        _ ->
            invalid
    end;
json_term_to_reply(_) ->
    invalid.
