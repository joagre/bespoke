% -*- fill-column: 100; -*-

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

-define(READ_CACHE_FILENAME, filename:join(?DB_DIR, "readCache.db")).
-define(READ_CACHE_DB, read_cache_db).

-define(CHALLENGE_CACHE, challenge_cache).
-define(CHALLENGE_TIMEOUT, 5 * 60). % 5 minutes

-type(timestamp() :: integer()).

-record(state, {
                subscriptions = #{} ::
                  #{rester_socket() => {Request :: term(), db_serv:subscription_id()}}
               }).

-record(challenge_cache_entry, {
                                username :: db_user_serv:username(),
                                challenge :: webapp_crypto:challenge(),
                                timestamp :: timestamp()
                               }).

%%
%% Exported: start_link
%%

start_link() ->
    ok = webapp_dnsmasq:clear_all_mac_addresses(),
    Options =
	[{request_module, ?MODULE},
         {verify, verify_none},
         {cacertfile, filename:join([code:priv_dir(webapp), "b3s.zone/cacerts.pem"])},
         {certfile, filename:join([code:priv_dir(webapp), "b3s.zone/server.crt"])},
         {keyfile, filename:join([code:priv_dir(webapp), "b3s.zone/server.key"])},
	 {nodelay, true},
	 {reuseaddr, true}],
    ok = db_serv:open_disk_db(?READ_CACHE_DB, ?READ_CACHE_FILENAME, #read_cache.user_id),
    ok = db_serv:open_ram_db(?CHALLENGE_CACHE, #challenge_cache_entry.username),
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
    BaseDir = filename:join([code:lib_dir(main), "../.."]),
    TargetBinDir = filename:join([BaseDir, "target/bin"]),
    ScriptPath = filename:join([TargetBinDir, "change_ssid.sh"]),
    Command = lists:flatten(io_lib:format("sudo bash ~s \"~s\" 2>&1; echo $?", [ScriptPath, SSID])),
    ?log_info("Calling: ~s\n", [Command]),
    case string:trim(os:cmd(Command)) of
        "0" ->
            main:insert_config("SSID", ?b2l(SSID));
        UnexpectedOutput ->
            ?log_error("Unexpected output from change_ssid.sh (this is OK on a developer machine): ~s", [UnexpectedOutput]),
            {error, UnexpectedOutput}
    end.

%%
%% Exported: init callback
%%

init(_Socket, _Options) ->
    %%?log_info("init: ~p", [Options]),
    {ok, #state{}}.

%%
%% Exported: info callback
%%

info(Socket, {subscription_change, SubscriptionId, PostId}, State) ->
    ?log_info("Post ~s has been changed", [PostId]),
    case maps:get(Socket, State#state.subscriptions, not_found) of
        not_found ->
            {ok, State};
        {Request, SubscriptionId} ->
            send_response(Socket, Request, no_cache_headers(), {json, PostId}),
            UpdatedState = State#state{subscriptions =
                                           maps:remove(Socket, State#state.subscriptions)},
            {ok, UpdatedState};
        SpuriousSubscriptionId ->
            ?log_error("Spurious subscription id ~p\n", [SpuriousSubscriptionId]),
            {ok, State}
    end;
info(_Socket, _Info, State) ->
    %%?log_info("info: ~p\n", [{Info, State}]),
    {ok, State}.

%%
%% Exported: close callback
%%

close(_Socket, State) ->
    %%?log_info("close: ~p\n", [State]),
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
            ?log_info("Body = ~p", [Body]);
        true ->
            ok
    end,
    try http_request_(Socket, Request, Body, State) of
	Result ->
            Result
    catch
	_Class:Reason:StackTrace ->
	    ?log_error("http_request crashed: ~p\n~p\n", [Reason, StackTrace]),
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
            send_response(Socket, Request, no_cache_headers(), not_allowed)
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
        %% Bespoke API
        ["api", "auto_login"] ->
            case filelib:is_regular(filename:join([?RUNTIME_DIR, "bootstrap"])) of
                true ->
                    send_response(Socket, Request, no_cache_headers(), {found, "/bootstrap.html"});
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
                            send_response(Socket, Request, no_cache_headers(),
                                          {json, PayloadJsonTerm});
                        _ ->
                            UpdatedPayloadJsonTerm =
                                maps:put(<<"noPassword">>, false, PayloadJsonTerm),
                            send_response(Socket, Request, no_cache_headers(),
                                          {json, UpdatedPayloadJsonTerm})
                    end
            end;
        ["api", "list_top_posts"] ->
            case handle_request(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, _Body} ->
                    TopPosts = db_serv:list_top_posts(),
                    ReadPostIds = lookup_read_cache(UserId),
                    PayloadJsonTerm =
                        lists:map(
                          fun(#post{id = PostId} = Post) ->
                                  PostJsonTerm = post_to_json_term(Post, ReadPostIds),
                                  Posts = db_serv:lookup_posts([Post#post.id], recursive),
                                  ReplyPosts = lists:keydelete(PostId, #post.id, Posts),
                                  ReadCount = count_read_replies(ReadPostIds, ReplyPosts),
                                  maps:put(<<"readCount">>, ReadCount, PostJsonTerm)
                          end, TopPosts),
                    send_response(Socket, Request, no_cache_headers(), {json, PayloadJsonTerm})
            end;
        ["api", "list_files"] ->
            case handle_request(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                {ok, _User, _Body} ->
                    Files = db_serv:list_files(),
                    PayloadJsonTerm = lists:map(fun(File) -> file_to_json_term(File) end, Files),
                    send_response(Socket, Request, no_cache_headers(), {json, PayloadJsonTerm})
            end;
        %% Act as static web server
        Tokens ->
            UriPath =
                case Tokens of
                    [] ->
                        "/not_found.html";
                    _ ->
                        Url#url.path
                end,
            Filename = uri_string:unquote(tl(UriPath)),
            AbsFilename =
                filename:join([filename:absname(code:priv_dir(webapp)), "docroot", Filename]),
            AcceptEncoding = proplists:get_value('Accept-Encoding', Headers#http_chdr.other, ""),
            case string:str(AcceptEncoding, "gzip") of
                0 ->
                    case filelib:is_regular(AbsFilename) of
                        true ->
                            send_response(Socket, Request,
                                          [{content_type, {url, UriPath}}|
                                           no_cache_headers(AbsFilename)],
                                          {ok, {file, AbsFilename}});
                        false ->
                            send_response(Socket, Request, no_cache_headers(), not_found)
                    end;
                _ ->
                    GzippedAbsFilename = AbsFilename ++ ".gz",
                    case filelib:is_regular(GzippedAbsFilename) of
                        true ->
                            send_response(Socket, Request, [{content_type, {url, UriPath}},
                                                            {"Content-Encoding", "gzip"}|
                                                            no_cache_headers(GzippedAbsFilename)],
                                          {ok, {file, GzippedAbsFilename}});
                        false ->
                            case filelib:is_regular(AbsFilename) of
                                true ->
                                    send_response(Socket, Request, [{content_type, {url, UriPath}}|
                                                                    no_cache_headers(AbsFilename)],
                                                  {ok, {file, AbsFilename}});
                                false ->
                                    send_response(Socket, Request, no_cache_headers(), not_found)
                            end
                    end
            end
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
        ["api", "bootstrap"] ->
            case handle_request(Socket, Request, Body,  fun json_term_to_bootstrap/1, false) of
                {return, Result} ->
                    Result;
                {ok, no_user, SSID} ->
                    ok = file:delete(filename:join([?RUNTIME_DIR, "bootstrap"])),
                    ok = change_ssid(SSID),
                    ok = main:insert_config("SSID", ?b2l(SSID)),
                    send_response(Socket, Request, no_cache_headers(), no_content)
            end;
        ["api", "get_ssid"] ->
            case handle_request(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                {ok, _User, _Body} ->
                    {ok, SSID} = main:lookup_config("SSID", "BespokeBBS"),
                    send_response(Socket, Request, no_cache_headers(), {json, ?l2b(SSID)})
            end;
        ["api", "generate_challenge"] ->
            case handle_request(Socket, Request, Body, fun json_term_to_binary/1, false) of
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
                    PayloadJsonTerm = #{<<"passwordSalt">> => base64:encode(PasswordSalt),
                                        <<"challenge">> => base64:encode(Challenge)},
                    true = add_challenge_to_cache(Username, Challenge),
                    send_response(Socket, Request, no_cache_headers(), {json, PayloadJsonTerm})
            end;
        ["api", "login"] ->
            case handle_request(Socket, Request, Body, fun json_term_to_login/1, false) of
                {return, Result} ->
                    Result;
                {ok, no_user, {Username, ClientResponse}} ->
                    login(Socket, Request, Username, ClientResponse)
            end;
        ["api", "switch_user"] ->
            case handle_request(Socket, Request, Body,
                                fun json_term_to_switch_user/1) of
                {return, Result} ->
                    Result;
                {ok, _User, {Username, PasswordSalt, PasswordHash, ClientResponse}} ->
                    switch_user(Socket, Request, Username, PasswordSalt, PasswordHash,
                                ClientResponse)
            end;
        ["api", "change_password"] ->
            case handle_request(Socket, Request, Body,
                                fun json_term_to_change_password/1) of
                {return, Result} ->
                    Result;
                {ok, User, {PasswordSalt, PasswordHash}} ->
                    change_password(Socket, Request, User, PasswordSalt, PasswordHash)
            end;
        ["api", "lookup_posts"] ->
            case handle_request(Socket, Request, Body, fun json_term_to_binary_list/1) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, PostIds} ->
                    Posts = db_serv:lookup_posts(PostIds),
                    ReadPostIds = lookup_read_cache(UserId),
                    PayloadJsonTerm = lists:map(fun(Post) ->
                                                        post_to_json_term(Post, ReadPostIds) end,
                                                Posts),
                    send_response(Socket, Request, no_cache_headers(), {json, PayloadJsonTerm})
            end;
        ["api", "lookup_recursive_posts"] ->
            case handle_request(Socket, Request, Body, fun json_term_to_binary_list/1) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, PostIds} ->
                    Posts = db_serv:lookup_posts(PostIds, recursive),
                    ReadPostIds = lookup_read_cache(UserId),
                    PayloadJsonTerm = lists:map(fun(Post) ->
                                                        post_to_json_term(Post, ReadPostIds)
                                                end, Posts),
                    send_response(Socket, Request, no_cache_headers(), {json, PayloadJsonTerm})
            end;
        ["api", "lookup_recursive_post_ids"] ->
            case handle_request(Socket, Request, Body, fun json_term_to_binary_list/1) of
                {return, Result} ->
                    Result;
                {ok, _User, PostIds} ->
                    PayloadJsonTerm = db_serv:lookup_post_ids(PostIds, recursive),
                    send_response(Socket, Request, no_cache_headers(), {json, PayloadJsonTerm})
            end;
        ["api", "insert_post"] ->
            case handle_request(Socket, Request, Body, fun json_term_to_post/1) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, Post} ->
                    UpdatedPost = Post#post{author = UserId},
                    case db_serv:insert_post(UpdatedPost) of
                        {ok, InsertedPost} ->
                            ReadPostIds = lookup_read_cache(UserId),
                            PayloadJsonTerm = post_to_json_term(InsertedPost, ReadPostIds),
                            send_response(Socket, Request, no_cache_headers(),
                                          {json, PayloadJsonTerm});
                        {error, invalid_post} ->
                            send_response(Socket, Request, no_cache_headers(), bad_request)
                    end
            end;
        ["api", "delete_post"] ->
            case handle_request(Socket, Request, Body, fun json_term_to_binary/1) of
                {return, Result} ->
                    Result;
                {ok, #user{name = Username}, PostId} ->
                    case db_serv:lookup_posts([PostId]) of
                        [#post{author = Username}] ->
                            case db_serv:delete_post(PostId) of
                                ok ->
                                    send_response(Socket, Request, no_cache_headers(), no_content);
                                {error, not_found} ->
                                    send_response(Socket, Request, no_cache_headers(), not_found)
                            end
                    end;
                {ok, _User, _PostId} ->
                    send_response(Socket, Request, no_cache_headers(), forbidden)
            end;
        ["api", "toggle_like"] ->
            case handle_request(Socket, Request, Body, fun json_term_to_binary/1) of
                {return, Result} ->
                    Result;
                {ok, User, PostId} ->
                    {ok, Likers} = db_serv:toggle_like(PostId, User#user.id),
                    PayloadJsonTerm = #{<<"liked">> => lists:member(User#user.id, Likers),
                                        <<"likesCount">> => length(Likers)},
                    send_response(Socket, Request, no_cache_headers(), {json, PayloadJsonTerm})
            end;
        ["api", "insert_file"] ->
            case handle_request(Socket, Request, Body, fun json_term_to_file/1) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, File} ->
                    UpdatedFile = File#file{uploader = UserId},
                    case db_serv:insert_file(UpdatedFile) of
                        {ok, InsertedFile} ->
                            PayloadJsonTerm = file_to_json_term(InsertedFile),
                            send_response(Socket, Request, no_cache_headers(),
                                          {json, PayloadJsonTerm});
                        {error, invalid_file} ->
                            send_response(Socket, Request, no_cache_headers(), bad_request)
                    end
            end;
        ["api", "delete_file"] ->
            case handle_request(Socket, Request, Body, fun json_term_to_integer/1) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, FileId} ->
                    case db_serv:lookup_files([FileId]) of
                        [#file{uploader = UserId}] ->
                            case db_serv:delete_file(FileId) of
                                ok ->
                                    send_response(Socket, Request, no_cache_headers(), no_content);
                                {error, not_found} ->
                                    send_response(Socket, Request, no_cache_headers(), not_found)
                            end
                    end;
                {ok, _User, _fileId} ->
                    send_response(Socket, Request, no_cache_headers(), forbidden)
            end;
        ["api", "file_uploaded"] ->
            case handle_request(Socket, Request, Body, fun json_term_to_integer/1) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, FileId} ->
                    case db_serv:lookup_files([FileId]) of
                        [#file{uploader = UserId}] ->
                            case db_serv:file_uploaded(FileId) of
                                ok ->
                                    send_response(Socket, Request, no_cache_headers(), no_content);
                                {error, not_found} ->
                                    send_response(Socket, Request, no_cache_headers(), not_found)
                            end
                    end;
                {ok, _User, _fileId} ->
                    send_response(Socket, Request, no_cache_headers(), forbidden)
            end;
        ["api", "subscribe_on_changes"] ->
            case handle_request(Socket, Request, Body, fun json_term_to_binary_list/1) of
                {return, Result} ->
                    Result;
                {ok, _User, PostIds} ->
                    SubscriptionId = db_serv:subscribe_on_changes(PostIds),
                    UpdatedState = State#state{subscriptions =
                                                   maps:put(Socket, {Request, SubscriptionId},
                                                            State#state.subscriptions)},
                    {ok, UpdatedState}
            end;
        ["api", Token] when Token == "upload_attachments" orelse Token == "upload_file" ->
            [PayloadJsonTerm] =
                lists:map(fun(#{filename := Filename,
                                unique_filename := UniqueFilename,
                                content_type := ContentType}) ->
                                  AbsPath = filename:join([<<"/tmp">>, UniqueFilename]),
                                  #{<<"filename">> => Filename,
                                    <<"absPath">> => AbsPath,
                                    <<"contentType">> => ContentType}
                          end, Body),
            send_response(Socket, Request, no_cache_headers(), {json, PayloadJsonTerm});
        ["api", "upload_read_cache"] ->
            case handle_request(Socket, Request, Body, fun json_term_to_binary_list/1) of
                {return, Result} ->
                    Result;
                {ok, #user{id = UserId}, PostIds} ->
                    case dets:lookup(?READ_CACHE_DB, UserId) of
                        [] ->
                            ok = dets:insert(?READ_CACHE_DB,
                                             #read_cache{user_id = UserId, post_ids = PostIds});
                        [#read_cache{post_ids = ExistingPostIds} = ReadCache] ->
                            ok = dets:insert(?READ_CACHE_DB,
                                             ReadCache#read_cache{
                                               user_id = UserId,
                                               post_ids = lists:usort(PostIds ++ ExistingPostIds)})
                    end,
                    send_response(Socket, Request, no_cache_headers(), no_content)
            end;
        _ ->
	    ?log_error("~p not found", [Tokens]),
            send_response(Socket, Request, no_cache_headers(), not_found)
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
            send_response(Socket, Request, no_cache_headers(), not_found)
    end.

%%
%% Authentication
%%

login(Socket, Request, Username, ClientResponse) ->
    maybe
        {ok, Challenge} ?= get_challenge_from_cache(Username),
        {ok, #user{password_salt = PasswordSalt, password_hash = PasswordHash}} ?=
            db_user_serv:get_user_from_username(Username),
        true ?= webapp_crypto:verify_client_response(ClientResponse, Challenge, PasswordHash),
        {ok, MacAddress} = get_mac_address(Socket),
        {ok, #user{id = UserId, session_id = SessionId}} ?=
            db_user_serv:login(Username, MacAddress, PasswordSalt, PasswordHash),
        PayloadJsonTerm = #{<<"userId">> => UserId,
                            <<"username">> => Username,
                            <<"sessionId">> => base64:encode(SessionId)},
        send_response(Socket, Request, no_cache_headers(), {json, PayloadJsonTerm})
    else
        _ ->
            send_response(Socket, Request, no_cache_headers(), forbidden)
    end.

switch_user(Socket, Request, Username, _PasswordSalt = not_set, _PasswordHash = not_set,
            _ClientResponse = not_set) ->
    {ok, MacAddress} = get_mac_address(Socket),
    case db_user_serv:switch_user(Username, MacAddress) of
        {ok, #user{id = UserId, session_id = SessionId}} ->
            PayloadJsonTerm = #{<<"userId">> => UserId,
                                <<"username">> => Username,
                                <<"sessionId">> => base64:encode(SessionId)},
            send_response(Socket, Request, no_cache_headers(), {json, PayloadJsonTerm});
        {error, failure} ->
            send_response(Socket, Request, no_cache_headers(), forbidden)
    end;
switch_user(Socket, Request, Username, PasswordSalt, PasswordHash, ClientResponse) ->
    case get_challenge_from_cache(Username) of
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
                            send_response(Socket, Request, no_cache_headers(), forbidden)
                    end;
                {ok, _} ->
                    send_response(Socket, Request, no_cache_headers(), forbidden);
                {error, not_found} ->
                    {ok, MacAddress} = get_mac_address(Socket),
                    switch_user_now(Socket, Request, Username, MacAddress, PasswordSalt,
                                    PasswordHash)
            end;
        {error, not_found} ->
            send_response(Socket, Request, no_cache_headers(), forbidden)
    end.

switch_user_now(Socket, Request, Username, MacAddress, PasswordSalt, PasswordHash) ->
    #user{id = UserId, session_id = SessionId} =
        db_user_serv:switch_user(Username, MacAddress, PasswordSalt, PasswordHash),
    PayloadJsonTerm =
        #{<<"userId">> => UserId,
          <<"username">> => Username,
          <<"sessionId">> => base64:encode(SessionId)},
    send_response(Socket, Request, no_cache_headers(), {json, PayloadJsonTerm}).

change_password(Socket, Request, #user{name = Username}, PasswordSalt, PasswordHash) ->
    {ok, MacAddress} = get_mac_address(Socket),
    case db_user_serv:change_password(Username, MacAddress, PasswordSalt, PasswordHash) of
        ok ->
            send_response(Socket, Request, no_cache_headers(), no_content);
        {error, failure} ->
            send_response(Socket, Request, no_cache_headers(), forbidden)
    end.

%%
%% Captive portal
%%

redirect_to_loader(Socket, Request) ->
    {ok, SSID} = main:lookup_config("SSID", "BespokeBBS"),
    Host = string:lowercase(SSID),
    Url = ?l2b(io_lib:format("https://~s.b3s.zone/loader.html", [Host])),
    Body = io_lib:format("<!DOCTYPE html><html><head><body><a href=\"~s\" target=\"_blank\">Click here</a></body></head></html>", [Url]),
    send_response(Socket, Request, no_cache_headers(), {found, ?b2l(Url), ?l2b(Body)}).

%%
%% Read cache
%%

lookup_read_cache(UserId) ->
    case dets:lookup(?READ_CACHE_DB, UserId) of
        [] ->
            [];
        [#read_cache{post_ids = PostIds}] ->
            PostIds
    end.

count_read_replies(_ReadPostIds, []) ->
    0;
count_read_replies(ReadPostIds, [#post{id = PostId}|Rest]) ->
    case lists:member(PostId, ReadPostIds) of
        true ->
            1 + count_read_replies(ReadPostIds, Rest);
        false ->
            count_read_replies(ReadPostIds, Rest)
    end.

%%
%% Challenge cache
%%

add_challenge_to_cache(UserId, Challenge) ->
    ets:insert(?CHALLENGE_CACHE, #challenge_cache_entry{username = UserId,
                                                        challenge = Challenge,
                                                        timestamp = timestamp()}).

get_challenge_from_cache(Username) ->
    true = purge_challenge_cache(),
    case ets:lookup(?CHALLENGE_CACHE, Username) of
        [] ->
            {error, not_found};
        [#challenge_cache_entry{challenge = Challenge}] ->
            {ok, Challenge}
    end.

purge_challenge_cache() ->
    Threshold = timestamp() - ?CHALLENGE_TIMEOUT,
    ets:foldl(
      fun(#challenge_cache_entry{username = Username, timestamp = Timestamp}, _Acc)
            when Timestamp < Threshold ->
              ets:delete(?CHALLENGE_CACHE, Username);
         (_, Acc) ->
              Acc
      end, true, ?CHALLENGE_CACHE).

%%
%% Marshalling
%%

json_term_to_integer(Int) when is_integer(Int) ->
    {ok, Int};
json_term_to_integer(_) ->
    {error, invalid}.

json_term_to_binary(Bin) when is_binary(Bin) ->
    {ok, Bin};
json_term_to_binary(_) ->
    {error, invalid}.

json_term_to_binary_list(List) when is_list(List) ->
    json_term_to_binary_list(List, []);
json_term_to_binary_list(_) ->
    {error, invalid}.

json_term_to_binary_list([], Acc) ->
    {ok, lists:reverse(Acc)};
json_term_to_binary_list([Bin|Rest], Acc) when is_binary(Bin) ->
    json_term_to_binary_list(Rest, [Bin|Acc]);
json_term_to_binary_list(_, _Acc) ->
    {error, invalid}.

json_term_to_bootstrap(#{<<"ssid">> := SSID}) when is_binary(SSID) ->
    {ok, SSID};
json_term_to_bootstrap(_) ->
    {error, invalid}.

json_term_to_login(#{<<"username">> := Username, <<"clientResponse">> := ClientResponse} = JsonTerm)
  when is_binary(Username) andalso is_binary(ClientResponse) ->
    case has_valid_keys([<<"username">>, <<"clientResponse">>], JsonTerm) of
        true ->
            {ok, {Username, base64:decode(ClientResponse)}};
        false ->
            {error, invalid}
    end;
json_term_to_login(_) ->
    {error, invalid}.

json_term_to_switch_user(#{<<"username">> := Username,
                           <<"passwordSalt">> := PasswordSalt,
                           <<"passwordHash">> := PasswordHash,
                           <<"clientResponse">> := ClientResponse} = JsonTerm)
  when is_binary(Username) andalso
       (PasswordSalt == null orelse is_binary(PasswordSalt)) andalso
       (PasswordHash == null orelse is_binary(PasswordHash)) andalso
       (ClientResponse == null orelse is_binary(ClientResponse)) ->
    case has_valid_keys([<<"username">>,
                       <<"passwordSalt">>,
                       <<"passwordHash">>,
                       <<"clientResponse">>], JsonTerm) of
        true ->
            {ok, {Username, base64decode(PasswordSalt), base64decode(PasswordHash),
                  base64decode(ClientResponse)}};
        false ->
            {error, invalid}
    end;
json_term_to_switch_user(_) ->
    {error, invalid}.

base64decode(null) ->
    not_set;
base64decode(Value) ->
    base64:decode(Value).

json_term_to_change_password(#{<<"passwordSalt">> := PasswordSalt,
                               <<"passwordHash">> := PasswordHash} = JsonTerm)
  when is_binary(PasswordHash) andalso is_binary(PasswordSalt) ->
    case has_valid_keys([<<"passwordSalt">>, <<"passwordHash">>], JsonTerm) of
        true ->
            {ok, {base64:decode(PasswordSalt), base64:decode(PasswordHash)}};
        false ->
            {error, invalid}
    end;
json_term_to_change_password(_) ->
    {error, invalid}.

%% Top post
json_term_to_post(#{<<"title">> := Title, <<"body">> := Body} = PostJsonTerm)
  when is_binary(Title) andalso is_binary(Body) ->
    case has_valid_keys([<<"title">>,
                         <<"body">>,
                         <<"created">>,
                         <<"attachments">>], PostJsonTerm) of
        true ->
            case {maps:get(<<"created">>, PostJsonTerm, not_set),
                  maps:get(<<"attachments">>, PostJsonTerm, [])} of
                {Created, AttachmentsJsonTerm}
                  when (Created == not_set orelse is_integer(Created)) andalso
                       (AttachmentsJsonTerm == [] orelse is_list(AttachmentsJsonTerm)) ->
                    case json_term_to_attachments(AttachmentsJsonTerm) of
                        {ok, Attachments} ->
                            {ok, #post{title = Title,
                                       body = Body,
                                       created = Created,
                                       attachments = Attachments}};
                        {error, invalid} ->
                            {error, invalid}
                    end;
                _ ->
                    {error, invalid}
            end;
        false ->
            {error, invalid}
    end;
%% Reply post
json_term_to_post(#{<<"parentPostId">> := ParentPostId,
                    <<"topPostId">> := TopPostId,
                    <<"body">> := Body} = PostJsonTerm)
  when is_binary(ParentPostId) andalso
       is_binary(TopPostId) andalso
       is_binary(Body) ->
    case has_valid_keys([<<"parentPostId">>,
                       <<"topPostId">>,
                       <<"body">>,
                       <<"created">>,
                       <<"attachments">>], PostJsonTerm) of
        true ->
            case {maps:get(<<"created">>, PostJsonTerm, not_set),
                  maps:get(<<"attachments">>, PostJsonTerm, [])} of
                {Created, AttachmentsJsonTerm}
                  when (Created == not_set orelse is_integer(Created)) andalso
                       (AttachmentsJsonTerm == [] orelse is_list(AttachmentsJsonTerm)) ->
                    case json_term_to_attachments(AttachmentsJsonTerm) of
                        {ok, Attachments} ->
                            {ok, #post{parent_post_id = ParentPostId,
                                       top_post_id = TopPostId,
                                       body = Body,
                                       created = Created,
                                       attachments = Attachments}};
                        {error, invalid} ->
                            {error, invalid}
                    end;

                _ ->
                    {error, invalid}
            end;
        false ->
            {error, invalid}
    end;
json_term_to_post(_) ->
    {error, invalid}.

json_term_to_file(#{<<"filename">> := Filename,
                    <<"size">> := Size,
                    <<"contentType">> := ContentType,
                    <<"isUploading">> := IsUploading} = FileJsonTerm)
  when is_binary(Filename) andalso
       is_integer(Size) andalso
       is_binary(ContentType) andalso
       is_boolean(IsUploading) ->
    case has_valid_keys([<<"filename">>,
                         <<"size">>,
                         <<"contentType">>,
                         <<"isUploading">>], FileJsonTerm) of
        true ->
            {ok, #file{filename = Filename,
                       size = Size,
                       content_type = ContentType,
                       is_uploading = IsUploading}};
        false ->
            {error, invalid}
    end.

json_term_to_attachments(AttachmentsJsonTerm) ->
    json_term_to_attachments(AttachmentsJsonTerm, []).

json_term_to_attachments([], Acc) ->
    {ok, lists:reverse(Acc)};
json_term_to_attachments([AttachmentJsonTerm|Rest], Acc) ->
    case json_term_to_attachment(AttachmentJsonTerm) of
        {ok, Attachment} ->
            json_term_to_attachments(Rest, [Attachment|Acc]);
        {error, invalid} ->
            {error, invalid}
    end.

json_term_to_attachment(#{<<"filename">> := Filename,
                          <<"contentType">> := ContentType} = Attachment)
  when is_binary(Filename) andalso is_binary(ContentType) ->
    case has_valid_keys([<<"filename">>, <<"contentType">>], Attachment) of
        true ->
            {ok, {Filename, ContentType}};
        false ->
            {error, invalid}
    end;
json_term_to_attachment(_) ->
    {error, invalid}.

post_to_json_term(#post{id = Id,
                        title = Title,
                        parent_post_id = ParentPostId,
                        top_post_id = TopPostId,
                        body = Body,
                        author = AuthorId,
                        created = Created,
                        reply_count = ReplyCount,
                        replies = Replies,
                        likers = Likers,
                        attachments = AttachmentsJsonTerm}, ReadPostIds) ->
    {ok, #user{name = AuthorUsername}} = db_user_serv:get_user(AuthorId),
    JsonTerm = #{<<"id">> => Id,
                 <<"body">> => Body,
                 <<"authorId">> => AuthorId,
                 <<"authorUsername">> => AuthorUsername,
                 <<"created">> => Created,
                 <<"replyCount">> => ReplyCount,
                 <<"replies">> => Replies,
                 <<"likers">> => Likers,
                 <<"attachments">> => attachments_to_json_term(AttachmentsJsonTerm),
                 <<"isRead">> => lists:member(Id, ReadPostIds)},
    add_optional_members([{<<"title">>, Title},
                          {<<"parentPostId">>, ParentPostId},
                          {<<"topPostId">>, TopPostId}], JsonTerm).

file_to_json_term(#file{id = Id,
                        filename = Filename,
                        size = Size,
                        uploaded_size = UploadedSize,
                        content_type = ContentType,
                        uploader = UploaderId,
                        created = Created,
                        is_uploading = IsUploading}) ->
    {ok, #user{name = UploaderUsername}} = db_user_serv:get_user(UploaderId),
    #{<<"id">> => Id,
      <<"filename">> => Filename,
      <<"size">> => Size,
      <<"uploadedSize">> => UploadedSize,
      <<"contentType">> => ContentType,
      <<"uploaderId">> => UploaderId,
      <<"uploaderUsername">> => UploaderUsername,
      <<"created">> => Created,
      <<"isUploading">> => IsUploading}.

attachments_to_json_term([]) ->
    [];
attachments_to_json_term([Attachment|Rest]) ->
    [attachment_to_json_term(Attachment)|attachments_to_json_term(Rest)].

attachment_to_json_term({Filename, ContentType}) ->
    #{<<"filename">> => Filename,
      <<"contentType">> => ContentType}.

add_optional_members([], JsonTerm) ->
    JsonTerm;
add_optional_members([{_Key, not_set}|Rest], JsonTerm) ->
    add_optional_members(Rest, JsonTerm);
add_optional_members([{Key, Value}|Rest], JsonTerm) ->
    add_optional_members(Rest, maps:put(Key, Value, JsonTerm)).

%%
%% Utilities
%%

timestamp() ->
    os:system_time(second).

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
    {errnor, not_found};
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

handle_request(Socket, Request, Body) ->
    handle_request(Socket, Request, Body, undefined, true).

handle_request(Socket, Request, Body, MarshallingFun) ->
    handle_request(Socket, Request, Body, MarshallingFun, true).

handle_request(Socket, Request, Body, MarshallingFun, Authenticate) ->
    case authenticate(Request, Authenticate) of
        {ok, User} ->
            case (Request#http_request.headers)#http_chdr.content_type of
                undefined ->
                    {ok, User, no_body};
                _ ->
                    case rest_util:parse_body(Request, Body) of
                        {error, _Reason} ->
                            {return,
                             send_response(Socket, Request, no_cache_headers(), bad_request)};
                        ParsedBody when is_function(MarshallingFun) ->
                            case MarshallingFun(ParsedBody) of
                                {ok, MarshalledBody} ->
                                    {ok, User, MarshalledBody};
                                {error, Reason} ->
                                    ?log_error(Reason),
                                    {return, send_response(Socket, Request, no_cache_headers(),
                                                           bad_request)}
                            end
                    end
            end;
        {error, not_found} ->
            {return, send_response(Socket, Request, no_cache_headers(), unauthorized)}
    end.

authenticate(_Request, false) ->
    {ok, no_user};
authenticate(Request, true) ->
    {ok, #{<<"sessionId">> := SessionId}} = get_bespoke_cookie(Request),
    db_user_serv:get_user_from_session_id(base64:decode(SessionId)).

has_valid_keys(PossibleKeys, Map) ->
    lists:all(fun(Key) -> lists:member(Key, PossibleKeys) end, maps:keys(Map)).

%%
%% HTTP response (rest_util:response/3 is just too unwieldly)
%%

send_response(Socket, Request, Opts, {ok, Body}) ->
    ?log_info("Response: ~p", [Body]),
    rester_http_server:response_r(Socket, Request, 200, "OK", Body, Opts);
send_response(Socket, Request, Opts, {json, JsonTerm}) ->
    ?log_info("Response: ~p", [JsonTerm]),
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
