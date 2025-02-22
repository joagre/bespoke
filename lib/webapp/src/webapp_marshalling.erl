% -*- fill-column: 100; -*-

-module(webapp_marshalling).
-export([decode/2, encode/2]).

-include_lib("apptools/include/log.hrl").
-include_lib("db/include/db.hrl").

-type decode_type() :: create_message | read_reply_messages | delete_message.
-type encode_type() :: create_message |
                       read_reply_messages |
                       read_top_messages |
                       delete_message.

%%
%% Exported: decode
%%

-spec decode(decode_type(), term()) -> {ok, term()} | {error, invalid}.

decode(create_message, #{<<"bodyBlobs">> := BodyBlobs} = JsonTerm) ->
    case valid_keys([<<"title">>,
                     <<"topMessageId">>,
                     <<"bodyBlobs">>,
                     <<"attachmentBlobs">>], JsonTerm) of
        true ->
            Title = maps:get(<<"title">>, JsonTerm, not_set),
            TopMessageId = maps:get(<<"topMessageId">>, JsonTerm, not_set),
            case {Title, TopMessageId} of
                {not_set, not_set} ->
                    {error, invalid};
                {Title, TopMessageId} when is_binary(Title) andalso is_binary(TopMessageId) ->
                    {error, invalid};
                _ ->
                    AttachmentBlobs = maps:get(<<"attachmentsBlobs">>, JsonTerm, []),
                    maybe
                        {ok, DecodedBodyBlobs} ?= decode_blobs(BodyBlobs),
                        {ok, DecodedAttachmentBlobs} ?= decode_blobs(AttachmentBlobs),
                        {ok, {#message{title = Title, top_message_id = TopMessageId},
                              DecodedBodyBlobs,
                              DecodedAttachmentBlobs}}
                    else
                        Error ->
                            {error, invalid}
                    end
            end;
        false ->
            {error, invalid}
    end;
decode(read_reply_messages, JsonTerm) ->
    decode_integer_list(JsonTerm);
decode(delete_message, MessageId) ->
    decode_integer(MessageId);
decode(_, _) ->
    {error, invalid}.

decode_blobs(Blobs) -> decode_blobs(Blobs, []).

decode_blobs([], Acc) ->
    {ok, lists:reverse(Acc)};
decode_blobs([Blobs|Rest], Acc) when is_list(Blobs) ->
    decode_blobs(Rest, [decode_blobs(Blobs)|Acc]);
decode_blobs([#{<<"userId">> := UserId, <<"filename">> := Filename}|Rest], Acc)
  when is_integer(UserId), is_binary(Filename) ->
    decode_blobs(Rest, [{UserId, Filename}|Acc]);
decode_blobs(_A, _B) ->
    {error, invalid}.

%%
%% Exported: encode
%%

-spec encode(encode_type(), term()) -> term().

encode(create_message, Message) ->
    encode_message(Message);
encode(read_reply_messages, Messages) ->
    lists:map(fun(Message) -> encode_message(Message) end, Messages);
encode(read_top_messages, Messages) ->
    lists:map(fun(Message) -> encode_message(Message) end, Messages).

encode_message(#message{id = Id,
                        title = Title,
                        top_message_id = TopMessageId,
                        author = AuthorId,
                        created = Created}) ->
    {ok, #user{name = AuthorUsername}} = db_user_serv:get_user(AuthorId),
    JsonTerm = #{<<"id">> => Id,
                 <<"authorId">> => AuthorId,
                 <<"authorUsername">> => AuthorUsername,
                 <<"created">> => Created},
    add_optional_members([{<<"title">>, Title}, {<<"topMessageId">>, TopMessageId}], JsonTerm).

%%
%% Utilities
%%

valid_keys(PossibleKeys, Map) ->
    lists:all(fun(Key) -> lists:member(Key, PossibleKeys) end, maps:keys(Map)).

add_optional_members([], JsonTerm) ->
    JsonTerm;
add_optional_members([{_Key, not_set}|Rest], JsonTerm) ->
    add_optional_members(Rest, JsonTerm);
add_optional_members([{Key, Value}|Rest], JsonTerm) ->
    add_optional_members(Rest, maps:put(Key, Value, JsonTerm)).

decode_integer_list(List) when is_list(List) ->
    decode_integer_list(List, []);
decode_integer_list(_) ->
    {error, invalid}.

decode_integer_list([], Acc) ->
    {ok, lists:reverse(Acc)};
decode_integer_list([Int|Rest], Acc) when is_integer(Int) ->
    decode_integer_list(Rest, [Int|Acc]);
decode_integer_list(_, _) ->
    {error, invalid}.

decode_integer(Int) when is_integer(Int) ->
    {ok, Int};
decode_integer(_) ->
    {error, invalid}.
