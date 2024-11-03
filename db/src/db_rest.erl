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

http_get(Socket, Request, Body, Options) ->
    Url = Request#http_request.uri,
    case string:tokens(Url#url.path, "/") of
	["versions"] ->
	    Object = json:encode([<<"v1">>]),
	    rester_http_server:response_r(Socket, Request, 200, "OK", Object,
                                          [{content_type, "application/json"}]);
	["v1"|Tokens] ->
	    http_get(Socket, Request, Options, Url, Tokens, Body, v1);
	Tokens ->
	    http_get(Socket, Request, Options, Url, Tokens,  Body, v1)
    end.

http_get(Socket, Request, _Options, Url, Tokens, _Body, v1) ->
    case Tokens of
        ["list_root_messages"] ->
            Messages = db_serv:list_root_messages(),
            JsonTerm = lists:map(fun(Message) ->
                                         message_to_json_term(Message)
                                 end, Messages),
            rest_util:response(Socket, Request, {ok, {format, JsonTerm}});
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
        ["lookup_messages"] ->
            case rest_util:parse_body(Request, Body) of
                {error, _Reason} ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"});
                MessageIds when is_list(MessageIds) ->
                    case lists:all(fun(MessageId) when is_integer(MessageId) ->
                                           true;
                                      (_) ->
                                           false
                                   end, MessageIds) of
                        true ->
                            Messages = db_serv:lookup_messages(MessageIds),
                            JsonTerm =
                                lists:map(fun(Message) ->
                                                  message_to_json_term(Message)
                                          end, Messages),
                            rest_util:response(
                              Socket, Request, {ok, {format, JsonTerm}});
                        false ->
                            rest_util:response(
                              Socket, Request,
                              {error, bad_request,
                               "message-ids must be integers"})
                    end;
                _ ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"})
            end;
        ["insert_message"] ->
            case rest_util:parse_body(Request, Body) of
                {error, _Reason} ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"});
                MessageJsonTerm ->
                    case json_term_to_message(MessageJsonTerm) of
                        {ok, Message} ->
                            {ok, InsertedMessage} =
                                db_serv:insert_message(Message),
                            InsertedMessageJsonTerm =
                                message_to_json_term(InsertedMessage),
                            rest_util:response(
                              Socket, Request,
                              {ok, {format, InsertedMessageJsonTerm}});
                        {error, invalid} ->
                            rest_util:response(
                              Socket, Request,
                              {error, bad_request, "Invalid message"})
                    end
            end;
        ["delete_message"] ->
            case rest_util:parse_body(Request, Body) of
                {error, _Reason} ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"});
                [MessageId] when is_integer(MessageId) ->
                    case db_serv:delete_message(MessageId) of
                        ok ->
                            rest_util:response(Socket, Request, ok_204);
                        {error, not_found} ->
                            rest_util:response(Socket, Request,
                                               {error, not_found})
                    end;
                _ ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "message-id must be an integer"})
            end;
	_ ->
	    ?log_error("~p not found", [Tokens]),
	    rest_util:response(Socket, Request, {error, not_found})
    end.

%%
%% Marshalling
%%

message_to_json_term(#message{id = Id,
                              title = Title,
                              reply_message_id = ReplyMessageId,
                              root_message_id = RootMessageId,
                              body = Body,
                              author = Author,
                              created = Created,
                              reply_count = ReplyCount,
                              replies = Replies}) ->
    #{<<"id">> => Id,
      <<"title">> => title_to_json_term(Title),
      <<"reply-message-id">> => id_to_json_term(ReplyMessageId),
      <<"root-message-id">> => id_to_json_term(RootMessageId),
      <<"body">> => ?l2b(Body),
      <<"author">> => ?l2b(Author),
      <<"created">> => Created,
      <<"reply-count">> => ReplyCount,
      <<"replies">> => Replies}.

title_to_json_term(not_set) ->
    "";
title_to_json_term(Title) ->
    ?l2b(Title).

id_to_json_term(not_set) ->
    -1;
id_to_json_term(Id) ->
    Id.

json_term_to_message(#{<<"title">> := Title,
                       <<"body">> := Body,
                       <<"author">> := Author} = MessageJsonTerm) ->
    case no_more_keys([<<"title">>, <<"body">>, <<"author">>],
                      MessageJsonTerm) of
        true ->
            {ok, #message{title = ?b2l(Title),
                          body = ?b2l(Body),
                          author = ?b2l(Author)}};
        false ->
            {error, invalid}
    end;
json_term_to_message(#{<<"reply-message-id">> := ReplyMessageId,
                       <<"root-message-id">> := RootMessageId,
                       <<"body">> := Body,
                       <<"author">> := Author} = MessageJsonTerm) ->
    case no_more_keys([<<"reply-message-id">>,
                       <<"root-message-id">>,
                       <<"body">>,
                       <<"author">>], MessageJsonTerm) of
        true ->
            {ok, #message{reply_message_id = ReplyMessageId,
                          root_message_id = RootMessageId,
                          body = ?b2l(Body),
                          author = ?b2l(Author)}};
        false ->
            {error, invalid}
    end;
json_term_to_message(_) ->
    {error, invalid}.

no_more_keys(RequiredKeys, Map) ->
    lists:sort(maps:keys(Map)) =:= lists:sort(RequiredKeys).
