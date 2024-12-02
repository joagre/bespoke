-module(db_user_serv).
-export([start_link/0, stop/0]).
-export([get_user/1, get_username/1, authenticate/3, mac_address_to_username/1,
         username_to_mac_address/1]).
-export([message_handler/1]).
-export_type([username/0, user_id/0, pwhash/0, session_id/0, password/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include("db.hrl").

-define(USER_DB_FILENAME, "users.db").
-define(USER_DB, users).
-define(WORD_LIST_PATH, "/usr/share/dict/words").
-define(SESSION_ID_SIZE, 16).

-type username() :: binary().
-type user_id() :: integer().
-type pwhash() :: binary().
-type session_id() :: binary().
-type password() :: binary().

-record(state, {
                parent :: pid(),
                word_list = []
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
%% Exported: get_user
%%

-spec get_user(db_dnsmasq:mac_address()) -> #user{}.

get_user(MacAddress) ->
    serv:call(?MODULE, {get_user, MacAddress}).

%%
%% Exported: get_username
%%

-spec get_username(db_dnsmasq:mac_address()) -> username().

get_username(MacAddress) ->
    serv:call(?MODULE, {get_username, MacAddress}).

%%
%% Exported: authenticate
%%

-spec authenticate(username(), password(), db_dnsmasq:mac_address()) ->
          {ok, session_id()} | {error, failure}.

authenticate(Username, Password, MacAddress) ->
    serv:call(?MODULE, {authenticate, Username, Password, MacAddress}).

%%
%% Exported: mac_address_to_username
%%

-spec mac_address_to_username(db_dnsmasq:mac_address()) ->
          {ok, username()} | {error, not_found}.

mac_address_to_username(MacAddress) ->
    serv:call(?MODULE, {mac_address_to_username, MacAddress}).

%%
%% Exported: username_to_mac_address
%%

-spec username_to_mac_address(username()) ->
          {ok, db_dnsmasq:mac_address()} | {error, not_found}.

username_to_mac_address(Username) ->
    serv:call(?MODULE, {username_to_mac_address, Username}).

%%
%% Server
%%

init(Parent) ->
    {ok, ?USER_DB} =
        dets:open_file(
          ?USER_DB,
          [{file, filename:join(code:priv_dir(db), ?USER_DB_FILENAME)},
           {keypos, #user.name}]),
    ?log_info("Database user server has been started"),
    {ok, #state{parent = Parent,
                word_list = init_word_list()}}.

message_handler(S) ->
    receive
        {call, From, stop = Call} ->
            ?log_debug("Call: ~p", [Call]),
            ok = dets:close(?USER_DB),
            {reply, From, ok};
        {call, From, {get_user, MacAddress}} ->
            ?log_debug("Call: ~p", [{get_user, MacAddress}]),
            case dets:match_object(?USER_DB,
                                   #user{mac_address = MacAddress, _ = '_'}) of
                [User] ->
                    SessionId = session_id(),
                    ok = dets:insert(?USER_DB, User#user{session_id = SessionId}),
                    {reply, From, User};
                [] ->
                    Username = generate_username(S#state.word_list),
                    UserId = db_serv:allocate_user_id(),
                    SessionId = session_id(),
                    User = #user{name = Username,
                                 id = UserId,
                                 session_id = SessionId,
                                 mac_address = MacAddress},
                    {reply, From, User}
            end;
        {call, From, {get_username, MacAddress}} ->
            ?log_debug("Call: ~p", [{get_username, MacAddress}]),
            case dets:match_object(?USER_DB,
                                   #user{mac_address = MacAddress, _ = '_'}) of
                [User|_] ->
                    {reply, From, User#user.name};
                [] ->
                    {reply, From, generate_username(S#state.word_list)}
            end;
        {call, From, {authenticate, Username, Password, MacAddress}} ->
            ?log_debug("Call: ~p",
                       [{authenticate, Username, Password, MacAddress}]),
            case dets:lookup(?USER_DB, Username) of
                [User] ->
                    ok;
                [] ->
                    User = #user{name = Username,
                                 id = db_serv:allocate_user_id(),
                                 mac_address = MacAddress},
                    ok = dets:insert(?USER_DB, User)
            end,
            case do_authenticate(User, Password) of
                {ok, SessionId} ->
                    {reply, From, {ok, SessionId}};
                {error, Reason} ->
                    {reply, From, {error, Reason}}
            end;
        {call, From, {mac_address_to_username, MacAddress}} ->
            ?log_debug("Call: ~p", [{mac_address_to_username, MacAddress}]),
            case dets:match_object(?USER_DB,
                                   #user{mac_address = MacAddress, _ = '_'}) of
                [User] ->
                    {reply, From, {ok, User#user.name}};
                [] ->
                    {reply, From, {error, not_found}}
            end;
        {call, From, {username_to_mac_address, Username}} ->
            ?log_debug("Call: ~p", [{username_to_mac_address, Username}]),
            case dets:lookup(?USER_DB, Username) of
                [#user{mac_address = MacAddress}] ->
                    {reply, From, {ok, MacAddress}};
                [] ->
                    {reply, From, {error, not_found}}
            end;
        {'EXIT', Pid, Reason} when S#state.parent == Pid ->
            exit(Reason);
        {system, From, Request} ->
            ?log_debug("System: ~p", [Request]),
            {system, From, Request};
        UnknownMessage ->
            ?log_error("Unknown message: ~p", [UnknownMessage]),
            noreply
    end.

%%
%% Call: get_username
%%

generate_username(WordList) ->
    Username = ?l2b([random_word(WordList), random_word(WordList)]),
    case dets:lookup(?USER_DB, Username) of
        [] ->
            Username;
        _ ->
            generate_username(WordList)
    end.

random_word(Words) ->
    Index = enacl:randombytes_uniform(length(Words) + 1),
    lists:nth(Index, Words).

%%
%% Call: authenticate
%%

%% Proceed without password
do_authenticate(#user{pwhash = not_set} = User, <<>>) ->
    SessionId = session_id(),
    ok = dets:insert(?USER_DB, User#user{session_id = SessionId}),
    {ok, SessionId};
%% Set a password
do_authenticate(#user{pwhash = not_set} = User, Password) ->
    Pwhash = enacl:pwhash_str(Password, interactive, interactive),
    SessionId = session_id(),
    ok = dets:insert(?USER_DB, User#user{pwhash = Pwhash,
                                         session_id = SessionId}),
    {ok, SessionId};
%% Verify password
do_authenticate(#user{pwhash = Pwhash} = User, Password) ->
    case enacl:pwhash_str_verify(Pwhash, Password) of
        true ->
            SessionId = session_id(),
            ok = dets:insert(?USER_DB, User#user{session_id = SessionId}),
            {ok, SessionId};
        false ->
            {error, failure}
    end.

session_id() ->
    ?l2b(base64:encode_to_string(enacl:randombytes(?SESSION_ID_SIZE))).

%%
%% Word list
%%

init_word_list() ->
    {ok, File} = file:open(?WORD_LIST_PATH, [read]),
    init_word_list(File).

init_word_list(File) ->
    case file:read_line(File) of
        {ok, Word} ->
            case is_valid_word(Word) of
                {true, ValidWord} ->
                    [ValidWord|init_word_list(File)];
                false ->
                    init_word_list(File)
            end;
        eof ->
            file:close(File),
            []
    end.

is_valid_word(Word) ->
    StrippedWord = string:strip(Word, right, $\n),
    case string:len(StrippedWord) of
        N when N > 1 ->
            case string:substr(StrippedWord, string:len(StrippedWord) - 1, 2) of
                "'s" ->
                    false;
                _ ->
                    %% We know that there are only ASCII words in the dictionary
                    {true, ?l2b([string:to_upper(hd(StrippedWord))|tl(StrippedWord)])}
            end;
        _ ->
            false
    end.
