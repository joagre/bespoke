-module(db_user_serv).
-export([start_link/0, stop/0]).
-export([get_user/1, get_user_from_username/1, get_user_from_session_id/1,
         get_user_from_mac_address/1,
         login/3, switch_user/3, change_password/4,
         user_db_to_list/0]).
-export([message_handler/1]).
-export_type([username/0, pwhash/0, mac_address/0, session_id/0, salt/0,
              password/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include("../include/db.hrl").

-define(USER_DB_FILENAME, "/var/tmp/users.db").
-define(USER_DB, users).
-define(WORD_LIST_PATH, "/usr/share/dict/words").
-define(SESSION_ID_SIZE, 16).

-type username() :: binary().
-type pwhash() :: binary().
-type mac_address() :: binary().
-type session_id() :: binary().
-type salt() :: binary().
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

-spec get_user(db_serv:user_id()) ->
          {ok, #user{}} | {error, not_found}.

get_user(UserId) ->
    serv:call(?MODULE, {get_user, UserId}).

%%
%% Exported: get_user_from_username
%%

-spec get_user_from_username(db_user_serv:username()) ->
          {ok, #user{}} | {error, not_found}.

get_user_from_username(Username) ->
    serv:call(?MODULE, {get_user_from_username, Username}).

%%
%% Exported: get_user_from_session_id
%%

-spec get_user_from_session_id(session_id()) ->
          {ok, #user{}} | {error, not_found}.

get_user_from_session_id(SessionId) ->
    serv:call(?MODULE, {get_user_from_session_id, SessionId}).

%%
%% Exported: get_user_from_mac_address
%%

-spec get_user_from_mac_address(mac_address()) -> #user{}.

get_user_from_mac_address(MacAddress) ->
    serv:call(?MODULE, {get_user_from_mac_address, MacAddress}).

%%
%% Exported: login
%%

-spec login(username(), salt(), pwhash()) ->
          {ok, #user{}} | {error, failure}.

login(Username, Salt, Pwhash) ->
    serv:call(?MODULE, {login, Username, Salt, Pwhash}).

%%
%% Exported: switch_user
%%

-spec switch_user(username(), password(), mac_address()) ->
          {ok, #user{}} | {error, failure}.

switch_user(Username, Password, MacAddress) ->
    serv:call(?MODULE, {switch_user, Username, Password, MacAddress}).

%%
%% Exported: change_password
%%

-spec change_password(username(), mac_address(), salt(), pwhash()) ->
          ok | {error, failure}.

change_password(Username, MacAddress, Salt, Pwhash) ->
    serv:call(?MODULE, {change_password, Username, MacAddress, Salt, Pwhash}).

%%
%% Exported: user_db_to_list
%%

-spec user_db_to_list() -> [#user{}].

user_db_to_list() ->
    serv:call(?MODULE, user_db_to_list).

%%
%% Server
%%

init(Parent) ->
    {ok, ?USER_DB} =
        dets:open_file(
          ?USER_DB, [{file, ?USER_DB_FILENAME}, {keypos, #user.id}]),
    ?log_info("Database user server has been started"),
    {ok, #state{parent = Parent,
                word_list = init_word_list()}}.

message_handler(S) ->
    receive
        {call, From, stop = Call} ->
            ?log_debug("Call: ~p", [Call]),
            ok = dets:close(?USER_DB),
            {reply, From, ok};
        {call, From, {get_user, UserId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:lookup(?USER_DB, UserId) of
                [User] ->
                    {reply, From, {ok, User}};
                [] ->
                    {reply, From, {error, not_found}}
            end;
        {call, From, {get_user_from_username, Username} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:match_object(?USER_DB,
                                   #user{name = Username, _ = '_'}) of
                [User] ->
                    {reply, From, {ok, User}};
                [] ->
                    {reply, From, {error, not_found}}
            end;
        {call, From, {get_user_from_session_id, SessionId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:match_object(?USER_DB,
                                   #user{session_id = SessionId, _ = '_'}) of
                [User] ->
                    {reply, From, {ok, User}};
                [] ->
                    {reply, From, {error, not_found}}
            end;
        {call, From, {get_user_from_mac_address, MacAddress} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:match_object(?USER_DB,
                                   #user{mac_address = MacAddress, _ = '_'}) of
                [] ->
                    %% Note: Generate a new user
                    User = #user{id = db_serv:get_user_id(),
                                 name = generate_username(S#state.word_list),
                                 mac_address = MacAddress,
                                 updated = timestamp(),
                                 session_id = session_id()},
                    ok = dets:insert(?USER_DB, User),
                    {reply, From, User};
                Users ->
                    [LastUpdatedUser|_] =
                        lists:sort(
                          fun(User1, User2) ->
                                  User1#user.updated > User2#user.updated
                          end, Users),
                    ok = dets:insert(?USER_DB, LastUpdatedUser),
                    {reply, From, LastUpdatedUser}
            end;
        {call, From, {login, Username, Salt, Pwhash} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:match_object(?USER_DB, #user{name = Username, _ = '_'}) of
                [User] ->
                    UpdatedUser =
                        User#user{session_id = session_id(),
                                  salt = Salt,
                                  pwhash = Pwhash,
                                  updated = timestamp()},
                    ok = dets:insert(?USER_DB, UpdatedUser),
                    {reply, From, {ok, UpdatedUser}};
                [] ->
                    {reply, From, {error, failure}}
            end;
        {call, From, {switch_user, Username, Password, MacAddress} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:match_object(?USER_DB, #user{name = Username, _ = '_'}) of
                [#user{pwhash = not_set} = User] when Password == <<>> ->
                    UpdatedUser = User#user{mac_address = MacAddress,
                                            updated = timestamp(),
                                            session_id = session_id()},
                    ok = dets:insert(?USER_DB, UpdatedUser),
                    {reply, From, {ok, UpdatedUser}};
                [#user{pwhash = not_set} = User] ->
                    UpdatedUser = User#user{pwhash = hash_password(Password),
                                            mac_address = MacAddress,
                                            updated = timestamp(),
                                            session_id = session_id()},
                    ok = dets:insert(?USER_DB, UpdatedUser),
                    {reply, From, {ok, UpdatedUser}};
                [#user{pwhash = Pwhash} = User] ->
                    case verify_password(Pwhash, Password) of
                        true ->
                            UpdatedUser =
                                User#user{mac_address = MacAddress,
                                          updated = timestamp(),
                                          session_id = session_id()},
                            ok = dets:insert(?USER_DB, UpdatedUser),
                            {reply, From, {ok, UpdatedUser}};
                        false ->
                            {reply, From, {error, failure}}
                    end;
                [] when Password == <<>> ->
                    User = #user{id = db_serv:get_user_id(),
                                 name = Username,
                                 mac_address = MacAddress,
                                 updated = timestamp(),
                                 session_id = session_id()},
                    ok = dets:insert(?USER_DB, User),
                    {reply, From, {ok, User}};
                [] ->
                    User = #user{id = db_serv:get_user_id(),
                                 name = Username,
                                 pwhash = hash_password(Password),
                                 mac_address = MacAddress,
                                 updated = timestamp(),
                                 session_id = session_id()},
                    ok = dets:insert(?USER_DB, User),
                    {reply, From, {ok, User}}
            end;
        {call, From,
         {change_password, Username, MacAddress, Salt, Pwhash} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:match_object(?USER_DB, #user{name = Username, _ = '_'}) of
                [User] ->
                    UpdatedUser =
                        User#user{mac_address = MacAddress,
                                  salt = Salt,
                                  pwhash = Pwhash},
                    ok = dets:insert(?USER_DB, UpdatedUser),
                    {reply, From, ok};
                [] ->
                    {reply, From, {error, failure}}
            end;
        {call, From, user_db_to_list = Call} ->
            ?log_debug("Call: ~p", [Call]),
            UserDb = dets:match_object(?USER_DB, #user{_ = '_'}),
            {reply, From, UserDb};
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
%% Generate username
%%

generate_username(WordList) ->
    Username = ?l2b([random_word(WordList), random_word(WordList)]),
    case dets:match_object(?USER_DB, #user{name = Username, _ = '_'}) of
        [] ->
            Username;
        _ ->
            generate_username(WordList)
    end.

random_word(Words) ->
    Index = rand:uniform(length(Words)),
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

%%
%% Utilities
%%

timestamp() ->
    os:system_time(second).

%%% REMOVE

hash_password(Password) ->
    enacl:pwhash_str(Password, interactive, interactive).

%% REMOVE
verify_password(not_set, <<>>) ->
    true;
verify_password(not_set, _Password) ->
    false;
verify_password(Pwhash, Password) ->
    enacl:pwhash_str_verify(Pwhash, Password).

session_id() ->
    crypto:strong_rand_bytes(?SESSION_ID_SIZE).
