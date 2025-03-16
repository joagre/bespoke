% -*- fill-column:s 100; -*-

-module(webapp_rest).
-export([start_link/0, change_ssid/1]).
%% rester_http_server callbacks
-export([init/2, info/3, close/2, error/3, http_request/4]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("rester/include/rester.hrl").
-include_lib("rester/include/rester_http.hrl").
-include_lib("rester/include/rester_socket.hrl").
-include_lib("db/include/db.hrl").
-include("webapp_crypto.hrl").

-define(MAX_RECIPIENTS, 10).

-record(state, {
                subscriptions = [] ::
                  [{db_subscription_db:subscription_id(), Request :: term()}]
               }).

%%
%% Exported: start_link
%%

start_link() ->
    ok = webapp_cache:open(),
    %% Start HTTP(S) servers
    Options =
	[{request_module, ?MODULE},
         {verify, verify_none},
         {cacertfile, filename:join([code:priv_dir(webapp), "b3s.zone/cacerts.pem"])},
         {certfile, filename:join([code:priv_dir(webapp), "b3s.zone/server.crt"])},
         {keyfile, filename:join([code:priv_dir(webapp), "b3s.zone/server.key"])},
	 {nodelay, true},
	 {reuseaddr, true}],
    {ok, HttpPort} = main:lookup_config("HttpPort", 80),
    {ok, _} = rester_http_server:start_link(HttpPort, Options),
    case main:lookup_config("HttpsPort", 443) of
	{ok, HttpPort} ->
            ?log_info("Database REST API has been started"),
	    ok;
	{ok, HttpsPort} ->
            ?log_info("Database REST API has been started"),
	    rester_http_server:start_link(HttpsPort, Options)
    end.

%%
%% Exported: change_ssid
%%

-spec change_ssid(binary()) -> ok | {error, string()}.

change_ssid(SSID) ->
    BaseDirPath = filename:join([code:lib_dir(main), "../.."]),
    TargetBinDirPath = filename:join([BaseDirPath, "target/bin"]),
    ScriptPath = filename:join([TargetBinDirPath, "change_ssid.sh"]),
    Command = lists:flatten(io_lib:format("sudo bash ~s \"~s\" 2>&1; echo $?", [ScriptPath, SSID])),
    ?log_info("Calling: ~s", [Command]),
    case string:trim(os:cmd(Command)) of
        "0" ->
            main:insert_config("SSID", ?b2l(SSID));
        UnexpectedOutput ->
            ?log_error("~s: ~s (this is OK on a developer machine)", [Command, UnexpectedOutput]),
            {error, UnexpectedOutput}
    end.

%%
%% Exported: init callback
%%

init(_Socket, Options) ->
    ?log_info("init: ~p", [Options]),
    {ok, #state{}}.

%%
%% Exported: info callback
%%

info(_Socket, stop_subscription, State) ->
    ?log_info("Subscription has been stopped"),
    {stop, normal, State};
info(Socket, {subscription_change, SubscriptionId, PostId}, State) ->
    ?log_info("Post ~s has been changed", [PostId]),
    case lists:keytake(SubscriptionId, 1, State#state.subscriptions) of
        false ->
            ?log_error("Spurious subscription id ~p", [SubscriptionId]),
            {ok, State};
        {value, {SubscriptionId, Request}, RemainingSubscriptions} ->
            _ = send_response(Socket, Request, {json, PostId}),
            UpdatedState = State#state{subscriptions = RemainingSubscriptions},
            ok = webapp_session_serv:subscription_ended(SubscriptionId),
            {stop, normal, UpdatedState}
    end;
info(_Socket, Info, State) ->
    ?log_info("info: ~p", [{Info, State}]),
    {ok, State}.

%%
%% Exported: close callback
%%

close(_Socket, State) ->
    ?log_info("close: ~p", [State]),
    {ok, State}.

%%
%% Exported: error callback
%%

error(_Socket, Error, State) ->
    ?log_error("error: ~p", [{Error, State}]),
    {stop, normal, State}.

%%
%% Exported: http_request callback
%%

http_request(Socket, Request, Body, State) ->
%    ?log_info("Request = ~s, Headers = ~s, Body = ~p",
%              [rester_http:format_request(Request),
%               rester_http:format_hdr(Request#http_request.headers), Body]),
    ?log_info("~s", [rester_http:format_request(Request)]),
    if
        size(Body) > 0 ->
            ok;
            %%?log_info("Body = ~p", [Body]);
        true ->
            ok
    end,
    try http_request_(Socket, Request, Body, State) of
	Result ->
            Result
    catch
	_Class:Reason:StackTrace ->
	    ?log_error("http_request crashed: ~p\n~p", [Reason, StackTrace]),
	    erlang:error(Reason)
    end.

http_request_(Socket, Request, Body, State) ->
    case Request#http_request.method of
	'GET' ->
	    http_get(Socket, Request, Body, State);
	'POST' ->
	    http_post(Socket, Request, Body, State);
        'PUT' ->
            http_put(Socket, Request, Body, State);
	_ ->
            send_response(Socket, Request, not_allowed)
    end.

%%
%% HTTP GET
%%

http_get(Socket, Request, Body, State) ->
    Url = Request#http_request.uri,
    case string:tokens(Url#url.path, "/") of
	["versions"] ->
	    Object = json:encode([<<"v1">>]),
            send_response(Socket, Request, [{content_type, "application/json"}|no_cache_headers()],
                          {ok, Object});
	["v1"|Tokens] ->
	    http_get(Socket, Request, Url, Tokens, Body, State, v1);
	Tokens ->
	    http_get(Socket, Request, Url, Tokens, Body, State, v1)
    end.

http_get(Socket, Request, Url, Tokens, Body, _State, v1) ->
    Headers = Request#http_request.headers,
    case Tokens of
        %% iOS captive portal
        ["hotspot-detect.html"] ->
            redirect_to_loader(Socket, Request);
        %% Windows captive portal
        ["connecttest.txt"] ->
            redirect_to_loader(Socket, Request);
        ["ncsi.txt"] ->
            redirect_to_loader(Socket, Request);
        %% In Firefox captive portal, the browser will check for a captive portal
        %% https://support.mozilla.org/en-US/kb/captive-portal
        ["success.html"] when Headers#http_chdr.host == "detectportal.firefox.com" ->
            redirect_to_loader(Socket, Request);
        ["success.txt"] when Headers#http_chdr.host == "detectportal.firefox.com" ->
            redirect_to_loader(Socket, Request);
        %% Android captive portal
        ["generate_204"] ->
            redirect_to_loader(Socket, Request);
        ["gen_204"] ->
            redirect_to_loader(Socket, Request);
        _ when Headers#http_chdr.host == "connectivity-check.ubuntu.com." orelse
               Headers#http_chdr.host == "connectivity-check.ubuntu.com" ->
            redirect_to_loader(Socket, Request);
        %% Authentication
        ["api", "auto_login"] ->
            case filelib:is_regular(filename:join([?BESPOKE_RUNTIME_DIR, "bootstrap"])) of
                true ->
                    send_response(Socket, Request, {found, "/bootstrap.html"});
                false ->
                    {ok, MacAddress} = get_mac_address(Socket),
                    User = db_user_serv:get_user_from_mac_address(MacAddress),
                    PayloadJsonTerm =
                        #{<<"noPassword">> => true,
                          <<"userId">> => User#user.id,
                          <<"username">> => User#user.name,
                          <<"sessionId">> => base64:encode(User#user.session_id)},
                    case User#user.password_hash of
                        not_set ->
                            send_response(Socket, Request, {json, PayloadJsonTerm});
                        _ ->
                            UpdatedPayloadJsonTerm =
                                maps:put(<<"noPassword">>, false, PayloadJsonTerm),
                            send_response(Socket, Request, {json, UpdatedPayloadJsonTerm})
                    end
            end;
        %% Direct messaging
        ["api", "read_top_messages"] ->
            case decode(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, _Body} ->
                    ReadMessageIds = webapp_cache:list_read_messages(UserId),
                    {ok, TopMessageBundles} = db_serv:read_top_messages(UserId),
                    UpdatedTopMessageBundles =
                        lists:map(
                          fun(#{message := #message{id = MessageId} = Message,
                                reply_message_ids := ReplyMessageIds,
                                attachment_ids := AttachmentIds}) ->
                                  #{message => Message,
                                    attachment_ids => AttachmentIds,
                                    reply_count => length(ReplyMessageIds),
                                    is_read => is_message_read([MessageId|ReplyMessageIds],
                                                               ReadMessageIds)}
                          end, TopMessageBundles),
                    JsonTerm =
                        webapp_marshalling:encode(read_top_messages, UpdatedTopMessageBundles),
                    send_response(Socket, Request, {json, JsonTerm})
            end;
        %% Forum
        ["api", "read_top_posts"] ->
            case decode(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, _Body} ->
                    ReadPostIds = webapp_cache:list_read_posts(UserId),
                    {ok, TopPosts} = db_serv:read_top_posts(),
                    UpdatedTopPosts =
                        lists:map(fun(#post{id = PostId} = Post) ->
                                          {ok, AllPosts} = db_serv:read_posts([PostId], recursive),
                                          ReplyPosts = lists:keydelete(PostId, #post.id, AllPosts),
                                          ReadCount = count_read_replies(ReadPostIds, ReplyPosts),
                                          {Post, ReadCount}
                                  end, TopPosts),
                    JsonTerm = webapp_marshalling:encode(read_top_posts,
                                                         {ReadPostIds, UpdatedTopPosts}),
                    send_response(Socket, Request, {json, JsonTerm})
            end;
        %% File sharing
        ["api", "read_files"] ->
            case decode(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                {ok, _User, _Body} ->
                    {ok, Files} = db_serv:read_files(),
                    JsonTerm = webapp_marshalling:encode(read_files, Files),
                    send_response(Socket, Request, {json, JsonTerm})
            end;
        %% Static file delivery
        Tokens ->
            UriPath =
                case Tokens of
                    [] ->
                        "/not_found.html";
                    _ ->
                        Url#url.path
                end,
            FilePath = uri_string:unquote(tl(UriPath)),
            AbsFilePath =
                filename:join([filename:absname(code:priv_dir(webapp)), "docroot", FilePath]),
            AcceptEncoding = proplists:get_value('Accept-Encoding', Headers#http_chdr.other, ""),
            case string:str(AcceptEncoding, "gzip") of
                0 ->
                    case filelib:is_regular(AbsFilePath) of
                        true ->
                            ok = stop_subscriptions(Request, AbsFilePath),
                            send_response(
                              Socket, Request, [{content_type, {url, UriPath}}|
                                                no_cache_headers(AbsFilePath)],
                              {ok, {file, AbsFilePath}});
                        false ->
                            send_response(Socket, Request, not_found)
                    end;
                _ ->
                    GzippedAbsFilePath = AbsFilePath ++ ".gz",
                    case filelib:is_regular(GzippedAbsFilePath) of
                        true ->
                            send_response(Socket, Request,
                                          [{content_type, {url, UriPath}},
                                           {"Content-Encoding", "gzip"}|
                                           no_cache_headers(GzippedAbsFilePath)],
                                          {ok, {file, GzippedAbsFilePath}});
                        false ->
                            case filelib:is_regular(AbsFilePath) of
                                true ->
                                    send_response(Socket, Request,
                                                  [{content_type, {url, UriPath}}|
                                                   no_cache_headers(AbsFilePath)],
                                                  {ok, {file, AbsFilePath}});
                                false ->
                                    send_response(Socket, Request, not_found)
                            end
                    end
            end
    end.

stop_subscriptions(Request, AbsFilePath) ->
    case apptools_mime:mime_type(?l2b(AbsFilePath)) of
        {ok, <<"text/html">>} ->
            case get_bespoke_cookie(Request) of
                {ok, #{<<"sessionId">> := SessionId}} ->
                    webapp_session_serv:stop_subscriptions(SessionId);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

%%
%% HTTP POST
%%

http_post(Socket, Request, Body, State) ->
    Url = Request#http_request.uri,
    case string:tokens(Url#url.path, "/") of
	["v1"|Tokens] ->
	    http_post(Socket, Request, Url, Tokens, Body, State, v1);
	Tokens ->
	    http_post(Socket, Request, Url, Tokens, Body, State, v1)
    end.

http_post(Socket, Request, _Url, Tokens, Body, State, v1) ->
    case Tokens of
        %% Bootstrap
        ["api", "bootstrap"] ->
            case decode(Socket, Request, Body, bootstrap, false) of
                {return, Result} ->
                    Result;
                {ok, no_user, SSID} ->
                    ok = file:delete(filename:join([?BESPOKE_RUNTIME_DIR, "bootstrap"])),
                    ok = change_ssid(SSID),
                    ok = main:insert_config("SSID", ?b2l(SSID)),
                    send_response(Socket, Request, no_content)
            end;
        %% SSID management
        ["api", "get_ssid"] ->
            case decode(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                {ok, _User, _Body} ->
                    {ok, SSID} = main:lookup_config("SSID", "BespokeBBS"),
                    JsonTerm = webapp_marshalling:encode(get_ssid, SSID),
                    send_response(Socket, Request, {json, JsonTerm})
            end;
        %% Authentication
        ["api", "generate_challenge"] ->
            case decode(Socket, Request, Body, generate_challenge, false) of
                {return, Result} ->
                    Result;
                {ok, no_user, Username} ->
                    case db_user_serv:get_user_from_username(Username) of
                        {ok, #user{password_salt = PasswordSalt}} ->
                            ok;
                        {error, not_found} ->
                            PasswordSalt = crypto:strong_rand_bytes(?CRYPTO_PWHASH_SALTBYTES)
                    end,
                    Challenge = webapp_crypto:generate_challenge(),
                    JsonTerm =
                        webapp_marshalling:encode(generate_challenge, {PasswordSalt, Challenge}),
                    ok = webapp_cache:add_challenge(Username, Challenge),
                    send_response(Socket, Request, {json, JsonTerm})
            end;
        ["api", "login"] ->
            case decode(Socket, Request, Body, login, false) of
                {return, Result} ->
                    Result;
                {ok, no_user, {Username, ClientResponse}} ->
                    login(Socket, Request, Username, ClientResponse)
            end;
        ["api", "switch_user"] ->
            case decode(Socket, Request, Body, switch_user) of
                {return, Result} ->
                    Result;
                {ok, _User, #{username := Username,
                              password_salt := PasswordSalt,
                              password_hash := PasswordHash,
                              client_response := ClientResponse}} ->
                    switch_user(Socket, Request, Username, PasswordSalt, PasswordHash,
                                ClientResponse)
            end;
        ["api", "change_password"] ->
            case decode(Socket, Request, Body, change_password) of
                {return, Result} ->
                    Result;
                {ok, User, {PasswordSalt, PasswordHash}} ->
                    change_password(Socket, Request, User, PasswordSalt, PasswordHash)
            end;
        %% Direct messaging
        ["api", "create_message"] ->
            case decode(Socket, Request, Body, create_message) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, #{message := Message,
                                           body_blobs := BodyBlobs,
                                           attachment_blobs := AttachmentBlobs}} ->
                    UpdatedMessage = Message#message{author = UserId},
                    case db_serv:create_message(UpdatedMessage, BodyBlobs, AttachmentBlobs) of
                        {ok, CreatedMessage} ->
                            JsonTerm = webapp_marshalling:encode(create_message, CreatedMessage),
                            send_response(Socket, Request, {json, JsonTerm});
                        {error, access_denied} ->
                            ?log_error("/api/create_message: ~p", [access_denied]),
                            send_response(Socket, Request, forbidden);
                        {error, Reason} ->
                            ?log_error("/api/create_message: ~p", [Reason]),
                            %%?log_error("/api/create_message: ~p", [file:format_error(Reason)]),
                            send_response(Socket, Request, bad_request)
                    end
            end;
        ["api", "read_reply_messages"] ->
            case decode(Socket, Request, Body, read_reply_messages) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, MessageIds} ->
                    case db_serv:read_reply_messages(UserId, MessageIds) of
                        {ok, ReplyMessages} ->
                            JsonTerm = webapp_marshalling:encode(read_reply_messages,
                                                                 ReplyMessages),
                            send_response(Socket, Request, {json, JsonTerm});
                        {error, Reason} ->
                            ?log_error("/api/read_messages: ~p", [Reason]),
                            send_response(Socket, Request, bad_request)
                    end
            end;
        ["api", "delete_message"] ->
            case decode(Socket, Request, Body, delete_message) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, MessageId} ->
                    case db_serv:delete_message(UserId, MessageId) of
                        ok ->
                            send_response(Socket, Request, no_content);
                        {error, access_denied} ->
                            ?log_error("/api/delete_message: ~p", [access_denied]),
                            send_response(Socket, Request, forbidden)
                    end
            end;
        ["api", "search_recipients"] ->
            case decode(Socket, Request, Body, search_recipients) of
                {return, Result} ->
                    Result;
                {ok, _User, {IgnoredUsernames, Query}} ->
                    Recipients =
                        db_user_serv:search_recipients(IgnoredUsernames, Query, ?MAX_RECIPIENTS),
                    JsonTerm = webapp_marshalling:encode(search_recipients, Recipients),
                    send_response(Socket, Request, {json, JsonTerm})
            end;
        %% Forum
        ["api", "create_post"] ->
            case decode(Socket, Request, Body, create_post) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, Post} ->
                    UpdatedPost = Post#post{author = UserId},
                    case db_serv:create_post(UpdatedPost) of
                        {ok, CreatedPost} ->
                            ReadPostIds = webapp_cache:list_read_posts(UserId),
                            JsonTerm = webapp_marshalling:encode(
                                         create_post, {CreatedPost, ReadPostIds}),
                            send_response(Socket, Request, {json, JsonTerm});
                        {error, invalid_post} ->
                            send_response(Socket, Request, bad_request)
                    end
            end;
        ["api", "read_posts"] ->
            case decode(Socket, Request, Body, read_posts) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, PostIds} ->
                    ReadPostIds = webapp_cache:list_read_posts(UserId),
                    {ok, Posts} = db_serv:read_posts(PostIds),
                    JsonTerm = webapp_marshalling:encode(read_posts, {ReadPostIds, Posts}),
                    send_response(Socket, Request, {json, JsonTerm})
            end;
        ["api", "read_recursive_posts"] ->
            case decode(Socket, Request, Body, read_recursive_posts) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, PostIds} ->
                    ReadPostIds = webapp_cache:list_read_posts(UserId),
                    {ok, Posts} = db_serv:read_posts(PostIds, recursive),
                    JsonTerm = webapp_marshalling:encode(
                                 read_recursive_posts, {ReadPostIds, Posts}),
                    send_response(Socket, Request, {json, JsonTerm})
            end;
        ["api", "read_recursive_post_ids"] ->
            case decode(Socket, Request, Body, read_recursive_post_ids) of
                {return, Result} ->
                    Result;
                {ok, _User, PostIds} ->
                    {ok, PostIds} = db_serv:read_post_ids(PostIds, recursive),
                    JsonTerm = webapp_marshalling:encode(read_recursive_post_ids, PostIds),
                    send_response(Socket, Request, {json, JsonTerm})
            end;
        ["api", "delete_post"] ->
            case decode(Socket, Request, Body, delete_post) of
                {return, Result} ->
                    Result;
                {ok, #user{name = Username}, PostId} ->
                    case db_serv:read_posts([PostId]) of
                        {ok, [#post{author = Username}]} ->
                            case db_serv:delete_post(PostId) of
                                ok ->
                                    send_response(Socket, Request, no_content);
                                {error, not_found} ->
                                    send_response(Socket, Request, not_found)
                            end
                    end;
                {ok, _User, _PostId} ->
                    send_response(Socket, Request, forbidden)
            end;
        ["api", "toggle_post_like"] ->
            case decode(Socket, Request, Body, toggle_post_like) of
                {return, Result} ->
                    Result;
                {ok, User, PostId} ->
                    {ok, Likers} = db_serv:toggle_post_like(PostId, User#user.id),
                    JsonTerm = webapp_marshalling:encode(
                                 toggle_post_like, {User#user.id, Likers}),
                    send_response(Socket, Request, {json, JsonTerm})
            end;
        %% File sharing
        ["api", "create_file"] ->
            case decode(Socket, Request, Body, create_file) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, File} ->
                    UpdatedFile = File#file{uploader = UserId},
                    case db_serv:create_file(UpdatedFile) of
                        {ok, CreatedFile} ->
                            JsonTerm = webapp_marshalling:encode(create_file, CreatedFile),
                            send_response(Socket, Request, {json, JsonTerm});
                        {error, invalid_file} ->
                            send_response(Socket, Request, bad_request)
                    end
            end;
        ["api", "delete_file"] ->
            case decode(Socket, Request, Body, delete_file) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, FileId} ->
                    {ok, [#file{uploader = UserId}]} = db_serv:read_files([FileId]),
                    case db_serv:delete_file(FileId) of
                        ok ->
                            send_response(Socket, Request, no_content);
                        {error, not_found} ->
                            send_response(Socket, Request, not_found)
                    end;
                {ok, _User, _fileId} ->
                    send_response(Socket, Request, forbidden)
            end;
        ["api", "file_is_uploaded"] ->
            case decode(Socket, Request, Body, file_is_uploaded) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, FileId} ->
                    {ok, [#file{uploader = UserId}]} = db_serv:read_files([FileId]),
                    case db_serv:file_is_uploaded(FileId) of
                        ok ->
                            send_response(Socket, Request, no_content);
                        {error, not_found} ->
                            send_response(Socket, Request, not_found)
                    end;
                {ok, _User, _fileId} ->
                    send_response(Socket, Request, forbidden)
            end;
        %% Subscription handling
        ["api", "subscribe_on_changes"] ->
            case decode(Socket, Request, Body, subscribe_on_changes) of
                {return, Result} ->
                    Result;
                {ok, #user{session_id = SessionId}, PostIds} ->
                    ok = webapp_session_serv:stop_subscriptions(SessionId),
                    SubscriptionId = db_serv:subscribe_on_changes(PostIds),
                    ok = webapp_session_serv:subscription_started(SessionId, SubscriptionId),
                    UpdatedState =
                        State#state{subscriptions =
                                        [{SubscriptionId, Request}|State#state.subscriptions]},
                    {ok, UpdatedState}
            end;
        %% File uploading
        ["api", "upload_file"] ->
            [#{filename := Filename,
               unique_filename := UniqueFilename,
               content_type := ContentType}] = Body,
            JsonTerm = webapp_marshalling:encode(
                         upload_file, #{filename => Filename,
                                        absPath => filename:join([<<"/tmp">>, UniqueFilename]),
                                        contentType => ContentType}),
            send_response(Socket, Request, {json, JsonTerm});
        %% Read cache
        ["api", "upload_read_cache"] ->
            case decode(Socket, Request, Body, upload_read_cache) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, PostIds} ->
                    ok = webapp_cache:mark_posts_as_read(UserId, PostIds),
                    send_response(Socket, Request, no_content)
            end;
        _ ->
	    ?log_error("~p not found", [Tokens]),
            send_response(Socket, Request, not_found)
    end.

%%
%% HTTP PUT
%%

http_put(Socket, Request, Body, State) ->
    Url = Request#http_request.uri,
    case string:tokens(Url#url.path, "/") of
	["v1"|Tokens] ->
	    http_put(Socket, Request, Url, Tokens, Body, State, v1);
	Tokens ->
	    http_put(Socket, Request, Url, Tokens, Body, State, v1)
    end.

http_put(Socket, Request, _Url, Tokens, _Body, _State, v1) ->
    case Tokens of
        _ ->
	    ?log_error("~p not found", [Tokens]),
            send_response(Socket, Request, not_found)
    end.

%%
%% Authentication
%%

login(Socket, Request, Username, ClientResponse) ->
    maybe
        {ok, Challenge} ?= webapp_cache:get_challenge(Username),
        {ok, #user{password_salt = PasswordSalt, password_hash = PasswordHash}} ?=
            db_user_serv:get_user_from_username(Username),
        true ?= webapp_crypto:verify_client_response(ClientResponse, Challenge, PasswordHash),
        {ok, MacAddress} = get_mac_address(Socket),
        {ok, #user{id = UserId, session_id = SessionId}} ?=
            db_user_serv:login(Username, MacAddress, PasswordSalt, PasswordHash),
        JsonTerm = webapp_marshalling:encode(login, #{user_id => UserId,
                                                      username => Username,
                                                      session_id => SessionId}),
        send_response(Socket, Request, {json, JsonTerm})
    else
        _ ->
            send_response(Socket, Request, forbidden)
    end.

switch_user(Socket, Request, Username, _PasswordSalt = not_set, _PasswordHash = not_set,
            _ClientResponse = not_set) ->
    {ok, MacAddress} = get_mac_address(Socket),
    case db_user_serv:switch_user(Username, MacAddress) of
        {ok, #user{id = UserId, session_id = SessionId}} ->
            JsonTerm = webapp_marshalling:encode(switch_user, #{user_id => UserId,
                                                                username => Username,
                                                                session_id => SessionId}),
            send_response(Socket, Request, {json, JsonTerm});
        {error, failure} ->
            send_response(Socket, Request, forbidden)
    end;
switch_user(Socket, Request, Username, PasswordSalt, PasswordHash, ClientResponse) ->
    case webapp_cache:get_challenge(Username) of
        {ok, Challenge} ->
            case db_user_serv:get_user_from_username(Username) of
                {ok, #user{password_hash = not_set}} ->
                    {ok, MacAddress} = get_mac_address(Socket),
                    switch_user_now(Socket, Request, Username, MacAddress, PasswordSalt,
                                    PasswordHash);
                {ok, #user{password_salt = PasswordSalt, password_hash = PasswordHash}} ->
                    case webapp_crypto:verify_client_response(ClientResponse, Challenge,
                                                              PasswordHash) of
                        true ->
                            {ok, MacAddress} = get_mac_address(Socket),
                            switch_user_now(Socket, Request, Username, MacAddress, PasswordSalt,
                                            PasswordHash);
                        false ->
                            send_response(Socket, Request, forbidden)
                    end;
                {ok, _} ->
                    send_response(Socket, Request, forbidden);
                {error, not_found} ->
                    {ok, MacAddress} = get_mac_address(Socket),
                    switch_user_now(Socket, Request, Username, MacAddress, PasswordSalt,
                                    PasswordHash)
            end;
        {error, not_found} ->
            send_response(Socket, Request, forbidden)
    end.

switch_user_now(Socket, Request, Username, MacAddress, PasswordSalt, PasswordHash) ->
    #user{id = UserId, session_id = SessionId} =
        db_user_serv:switch_user(Username, MacAddress, PasswordSalt, PasswordHash),
    JsonTerm = webapp_marshalling:encode(switch_user, #{user_id => UserId,
                                                        username => Username,
                                                        session_id => SessionId}),
    send_response(Socket, Request, {json, JsonTerm}).

change_password(Socket, Request, #user{name = Username}, PasswordSalt, PasswordHash) ->
    {ok, MacAddress} = get_mac_address(Socket),
    case db_user_serv:change_password(Username, MacAddress, PasswordSalt, PasswordHash) of
        ok ->
            send_response(Socket, Request, no_content);
        {error, failure} ->
            send_response(Socket, Request, forbidden)
    end.

%%
%% Captive portal
%%

redirect_to_loader(Socket, Request) ->
    {ok, SSID} = main:lookup_config("SSID", "BespokeBBS"),
    Host = string:lowercase(SSID),
    Url = ?l2b(io_lib:format("https://~s.b3s.zone/loader.html", [Host])),
    Body = io_lib:format("<!DOCTYPE html><html><head><body><a href=\"~s\" target=\"_blank\">Click here</a></body></head></html>", [Url]),
    send_response(Socket, Request, {found, ?b2l(Url), ?l2b(Body)}).

%%
%% Read cache
%%

count_read_replies(_ReadPostIds, []) ->
    0;
count_read_replies(ReadPostIds, [#post{id = PostId}|Rest]) ->
    case lists:member(PostId, ReadPostIds) of
        true ->
            1 + count_read_replies(ReadPostIds, Rest);
        false ->
            count_read_replies(ReadPostIds, Rest)
    end.

is_message_read([], _ReadMessageIds) ->
    true;
is_message_read([MessageId|Rest], ReadMessageIds) ->
    case lists:member(MessageId, ReadMessageIds) of
        true ->
            is_message_read(Rest, ReadMessageIds);
        false ->
            false
    end.

%%
%% Utilities
%%

get_mac_address(Socket) ->
    case rester_socket:peername(Socket) of
        {ok, {{127, 0, 0, 1}, _Port}} ->
            {ok, <<"00:00:00:00:00:00">>};
        {ok, {IpAddress, _Port}} ->
            get_mac_address_for_ip_address(IpAddress)

    end.

get_mac_address_for_ip_address(IpAddress) ->
    IpAddressString = inet:ntoa(IpAddress),
    Command = "ping -c 1 " ++ IpAddressString ++ "; ip neigh show | awk '/" ++ IpAddressString ++
        "/ {print $5}'",
    case string:trim(os:cmd(Command)) of
        "" ->
            {error, not_found};
        MacAddress ->
            {ok, ?l2b(MacAddress)}
    end.

get_bespoke_cookie(Request) ->
    get_cookie("bespoke", (Request#http_request.headers)#http_chdr.cookie).

get_cookie(_Name, []) ->
    {error, not_found};
get_cookie(Name, [Cookie|Rest]) ->
    case string:tokens(Cookie, "=") of
        [Name, Value] ->
            {ok, json:decode(uri_string:percent_decode(?l2b(Value)))};
        _ ->
            get_cookie(Name, Rest)
    end.

no_cache_headers(Filename) ->
    case filename:extension(Filename) of
        ".html" ->
            no_cache_headers();
        ".js" ->
            no_cache_headers();
        ".gz" ->
            no_cache_headers();
        _ ->
            []
    end.

no_cache_headers() ->
    [{"Cache-Control", "no-cache, no-store, must-revalidate"},
     {"Pragma", "no-cache"},
     {"Expires", 0}].

decode(Socket, Request, Body) ->
    decode(Socket, Request, Body, undefined, true).

decode(Socket, Request, Body, MarshallingFun) ->
    decode(Socket, Request, Body, MarshallingFun, true).

decode(Socket, Request, Body, MarshallingFun, Authenticate) ->
    case authenticate(Request, Authenticate) of
        {ok, User} ->
            case (Request#http_request.headers)#http_chdr.content_type of
                undefined ->
                    {ok, User, no_body};
                _ ->
                    case rest_util:parse_body(Request, Body) of
                        {error, Reason} ->
                            ?log_error(Reason),
                            {return, send_response(Socket, Request, bad_request)};
                        ParsedBody ->
                            case MarshallingFun of
                                undefined ->
                                    {ok, User, ParsedBody};
                                MarshallingType ->
                                    case webapp_marshalling:decode(MarshallingType, ParsedBody) of
                                        {ok, DecodedBody} ->
                                            {ok, User, DecodedBody};
                                        {error, Reason} ->
                                            ?log_error(Reason),
                                            {return, send_response(Socket, Request, bad_request)}
                                    end
                            end
                    end
            end;
        {error, not_found} ->
            {return, send_response(Socket, Request, unauthorized)}
    end.

authenticate(_Request, false) ->
    {ok, no_user};
authenticate(Request, true) ->
    case get_bespoke_cookie(Request) of
        {ok, #{<<"sessionId">> := SessionId}} ->
            db_user_serv:get_user_from_session_id(base64:decode(SessionId));
        {error, not_found} ->
            {error, not_found}
    end.

%%
%% HTTP response (rest_util:response/3 is just too unwieldly)
%%

send_response(Socket, Request, Response) ->
    send_response(Socket, Request, no_cache_headers(), Response).

send_response(Socket, Request, Opts, {ok, Body}) ->
    ?log_info("Response: ~p", [Body]),
    rester_http_server:response_r(Socket, Request, 200, "OK", Body, Opts);
send_response(Socket, Request, Opts, {json, JsonTerm}) ->
    ?log_info("JSON Response: ~p", [JsonTerm]),
    Body = json:encode(JsonTerm),
    rester_http_server:response_r(Socket, Request, 200, "OK", Body,
                                  [{content_type, "application/json"}|Opts]);
send_response(Socket, Request, Opts, no_content) ->
    ?log_info("Response: No Content"),
    rester_http_server:response_r(Socket, Request, 204, "No Content", "", Opts);
send_response(Socket, Request, Opts, {found, AbsPath}) ->
    ?log_info("Response: Found (~p)", [AbsPath]),
    rester_http_server:response_r(Socket, Request, 302, "Found", "",
                                  [{location, "/bootstrap.html"}|Opts]);
send_response(Socket, Request, Opts, {found, AbsPath, Body}) ->
    %% Note: Adding a body to a 302 response is not standard, but it works better in practice (at
    %% least for the captive portal)
    ?log_info("Response: Found (~p)", [AbsPath]),
    rester_http_server:response_r(Socket, Request, 302, "Found", Body,
                                  [{location, "/bootstrap.html"}|Opts]);
send_response(Socket, Request, Opts, bad_request) ->
    ?log_info("Response: Bad Request"),
    rester_http_server:response_r(Socket, Request, 400, "Bad Request", "", Opts);
send_response(Socket, Request, Opts, unauthorized) ->
    ?log_info("Response: Unauthorized"),
    rester_http_server:response_r(Socket, Request, 401, "Unauthorized", "", Opts);
send_response(Socket, Request, Opts, forbidden) ->
    ?log_info("Response: Forbidden"),
    rester_http_server:response_r(Socket, Request, 403, "Forbidden", "", Opts);
send_response(Socket, Request, Opts, not_found) ->
    ?log_info("Response: Not Found"),
    rester_http_server:response_r(Socket, Request, 404, "Not Found", "", Opts);
send_response(Socket, Request, Opts, not_allowed) ->
    ?log_info("Response: Method Not Allowed"),
    rester_http_server:response_r(Socket, Request, 405, "Method Not Allowed", "",
                                  [{<<"Allow">>, <<"GET, PUT, POST">>}|Opts]).
%send_response(Socket, Request, Opts, internal_error) ->
%    ?log_info("Response: Internal Error"),
%    rester_http_server:response_r(Socket, Request, 500, "Internal Error", "", Opts).
