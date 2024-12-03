-module(db_user_serv).
-export([start_link/0, stop/0]).
-export([get_user/1, get_user_from_session_id/1, get_user_from_mac_address/1,
         authenticate/2]).
-export([message_handler/1]).
-export_type([username/0, pwhash/0, session_id/0, password/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include("db.hrl").

-define(USER_DB_FILENAME, "users.db").
-define(USER_DB, users).
-define(WORD_LIST_PATH, "/usr/share/dict/words").
-define(SESSION_ID_SIZE, 16).

-type username() :: binary().
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

-spec get_user(db_user_serv:username()) ->
          {ok, #user{}} | {error, not_found}.

get_user(Username) ->
    serv:call(?MODULE, {get_user, Username}).

%%
%% Exported: get_user_from_session_id
%%

-spec get_user_from_session_id(session_id) ->
          {ok, #user{}} | {error, not_found}.

get_user_from_session_id(SessionId) ->
    serv:call(?MODULE, {get_user_from_session_id, SessionId}).

%%
%% Exported: get_user_from_mac_address
%%

-spec get_user_from_mac_address(db_dnsmasq:mac_address()) -> #user{}.

get_user_from_mac_address(MacAddress) ->
    serv:call(?MODULE, {get_user_from_mac_address, MacAddress}).

%%
%% Exported: authenticate
%%

-spec authenticate(username(), password()) ->
          {ok, #user{}} | {error, failure}.

authenticate(Username, Password) ->
    serv:call(?MODULE, {authenticate, Username, Password}).

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
        {call, From, {get_user, Username}} ->
            case dets:lookup(?USER_DB, Username) of
                [User] ->
                    {reply, From, {ok, User}};
                [] ->
                    {reply, From, {error, not_found}}
            end;
        {call, From, {get_user_from_session_id, SessionId}} ->
            ?log_debug("Call: ~p", [{get_user_from_session_id, SessionId}]),
            case dets:match_object(?USER_DB,
                                   #user{session_id = SessionId, _ = '_'}) of
                [User] ->
                    {reply, From, {ok, User}};
                [] ->
                    {reply, From, {error, not_found}}
            end;
        {call, From, {get_user_from_mac_address, MacAddress}} ->
            ?log_debug("Call: ~p", [{get_user_from_mac_address, MacAddress}]),
            case dets:match_object(?USER_DB,
                                   #user{mac_address = MacAddress, _ = '_'}) of
                [] ->
                    %% Generate a new user with a session id
                    Username = generate_username(S#state.word_list),
                    SessionId = session_id(),
                    User = #user{name = Username,
                                 mac_address = MacAddress,
                                 updated = os:system_time(second),
                                 session_id = SessionId},
                    {reply, From, User};
                Users ->
                    [LastUpdatedUser|_] =
                        lists:sort(
                          fun(User1, User2) ->
                                  User1#user.updated < User2#user.updated
                          end, Users),
                    SessionId = session_id(),
                    ok = dets:insert(?USER_DB,
                                     LastUpdatedUser#user{session_id = SessionId}),
                    {reply, From, LastUpdatedUser}
            end;
        {call, From, {authenticate, Username, Password}} ->
            ?log_debug("Call: ~p", [{authenticate, Username, Password}]),
            case dets:lookup(?USER_DB, Username) of
                [#user{pwhash = Pwhash} = User] ->
                    %% Verify password
                    case enacl:pwhash_str_verify(Pwhash, Password) of
                        true ->
                            SessionId = session_id(),
                            UpdatedUser = User#user{session_id = SessionId},
                            ok = dets:insert(?USER_DB, UpdatedUser),
                            {reply, From, {ok, UpdatedUser}};
                        false ->
                            {error, failure}
                    end;
                [] ->
                    {reply, From, {error, failure}}
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

session_id() ->
    ?l2b(base64:encode_to_string(enacl:randombytes(?SESSION_ID_SIZE))).

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
                    {true, ?l2b([string:to_upper(hd(StrippedWord))|
                                 tl(StrippedWord)])}
            end;
        _ ->
            false
    end.
