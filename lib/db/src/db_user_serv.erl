% -*- fill-column: 100; -*-

-module(db_user_serv).
-export([start_link/0, stop/0, get_user/1, get_user_from_username/1, get_user_from_session_id/1,
         get_user_from_mac_address/1, search_recipients/3, insert_user/1, login/4, switch_user/2,
         switch_user/4, change_password/4, user_db_to_list/0]).
-export([message_handler/1]).
-export_type([session_id/0, mac_address/0, password_salt/0, password_hash/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include("../include/db.hrl").

%% User DB
-define(USER_FILE_PATH, filename:join(?BESPOKE_DB_DIR, "user.db")).
-define(USER_DB, user_db).

-define(WORD_LIST_PATH, "/usr/share/dict/words").
-define(MAX_USERNAME_LENGTH, 8).
-define(SESSION_ID_SIZE, 16).

-type session_id() :: binary().
-type mac_address() :: binary().
-type password_salt() :: binary().
-type password_hash() :: binary().

-record(state, {
                parent :: pid(),
                word_list = []
               }).

%%
%% Exported: start_link
%%

-spec start_link() -> serv:spawn_server_result().

start_link() ->
    ?spawn_server(fun init/1, fun ?MODULE:message_handler/1, #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    serv:call(?MODULE, stop).

%%
%% Exported: get_user
%%

-spec get_user(db:user_id()) ->
          {ok, #user{}} | {error, not_found}.

get_user(UserId) ->
    serv:call(?MODULE, {get_user, UserId}).

%%
%% Exported: get_user_from_username
%%

-spec get_user_from_username(db:username()) ->
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

-spec get_user_from_mac_address(mac_address()) -> {boolean(), #user{}}.

get_user_from_mac_address(MacAddress) ->
    serv:call(?MODULE, {get_user_from_mac_address, MacAddress}).

%%
%% Exported: search_recipients
%%

-spec search_recipients([db:username()], main:bstring(), integer()) ->
          [#{username => db:username(), user_id => db:user_id(), ignored => boolean()}].

search_recipients(IgnoredUsernames, Query, N) ->
    serv:call(?MODULE, {search_recipients, IgnoredUsernames, Query, N}).

%%
%% Exported: insert_user
%%

-spec insert_user(db:username()) -> {ok, #user{}}.

insert_user(Username) ->
    serv:call(?MODULE, {insert_user, Username}).

%%
%% Exported: login
%%

-spec login(db:username(), mac_address(), password_salt(), password_hash()) ->
          {ok, #user{}} | {error, failure}.

login(Username, MacAddress, PasswordSalt, PasswordHash) ->
    serv:call(?MODULE, {login, Username, MacAddress, PasswordSalt, PasswordHash}).

%%
%% Exported: switch_user
%%

-spec switch_user(db:username(), mac_address()) ->
          {ok, {boolean(), #user{}}} | {error, failure}.

switch_user(Username, MacAddress) ->
    serv:call(?MODULE, {switch_user, Username, MacAddress}).

-spec switch_user(db:username(), mac_address(), password_salt(), password_hash()) ->
          {ok, {boolean(), #user{}}}.

switch_user(Username, MacAddress, PasswordSalt, PasswordHash) ->
    serv:call(?MODULE, {switch_user, Username, MacAddress, PasswordSalt, PasswordHash}).

%%
%% Exported: change_password
%%

-spec change_password(db:username(), mac_address(), password_salt(), password_hash()) ->
          ok | {error, failure}.

change_password(Username, MacAddress, PasswordSalt, PasswordHash) ->
    serv:call(?MODULE, {change_password, Username, MacAddress, PasswordSalt, PasswordHash}).

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
    ok = open_db(),
    ?log_info("Database user server has been started"),
    {ok, #state{parent = Parent, word_list = init_word_list()}}.

message_handler(S) ->
    receive
        {call, From, stop = Call} ->
            ?log_debug("Call: ~p", [Call]),
            ok = close_db(),
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
            case dets:match_object(?USER_DB, #user{name = Username, _ = '_'}) of
                [User] ->
                    {reply, From, {ok, User}};
                [] ->
                    {reply, From, {error, not_found}}
            end;
        {call, From, {get_user_from_session_id, SessionId} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:match_object(?USER_DB, #user{session_id = SessionId, _ = '_'}) of
                [User] ->
                    {reply, From, {ok, User}};
                [] ->
                    {reply, From, {error, not_found}}
            end;
        {call, From, {get_user_from_mac_address, MacAddress} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:match_object(?USER_DB, #user{mac_address = MacAddress, _ = '_'}) of
                [] ->
                    %% Note: Generate a new user
                    User = #user{id = db_serv:get_user_id(),
                                 name = generate_username(S#state.word_list),
                                 mac_address = MacAddress,
                                 updated = db:seconds_since_epoch(),
                                 session_id = session_id()},
                    ok = dets:insert(?USER_DB, User),
                    {reply, From, {true, User}};
                Users ->
                    [LastUpdatedUser|_] =
                        lists:sort(fun(User1, User2) ->
                                           User1#user.updated > User2#user.updated
                                   end, Users),
                    ok = dets:insert(?USER_DB, LastUpdatedUser),
                    {reply, From, {false, LastUpdatedUser}}
            end;
        {call, From, {search_recipients, IgnoredUsernames, Query, N} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            Recipients = get_recipients(IgnoredUsernames, Query, N, dets:first(?USER_DB)),
            IgnoredRecipients =
                lists:foldl(
                  fun(Username, Acc) ->
                          case dets:match_object(?USER_DB, #user{name = Username, _ = '_'}) of
                              [#user{id = UserId}] ->
                                  [#{username => Username, user_id => UserId, ignored => true}|Acc];
                              [] ->
                                  Acc
                          end
                  end, [], IgnoredUsernames),
            SortedRecipients = lists:sort(fun(#{username := Username1}, #{username := Username2}) ->
                                                  Username1 < Username2
                                          end, Recipients ++ IgnoredRecipients),
            {reply, From, SortedRecipients};
        {call, From, {insert_user, Username} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            User = #user{id = db_serv:get_user_id(),
                         name = Username,
                         mac_address = <<"00:00:00:00:00:00">>,
                         updated = db:seconds_since_epoch(),
                         session_id = session_id()},
            ok = dets:insert(?USER_DB, User),
            {reply, From, {ok, User}};
        {call, From, {login, Username, MacAddress, PasswordSalt, PasswordHash} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:match_object(?USER_DB, #user{name = Username, _ = '_'}) of
                [User] ->
                    UpdatedUser = User#user{session_id = session_id(),
                                            mac_address = MacAddress,
                                            password_salt = PasswordSalt,
                                            password_hash = PasswordHash,
                                            updated = db:seconds_since_epoch()},
                    ok = dets:insert(?USER_DB, UpdatedUser),
                    {reply, From, {ok, UpdatedUser}};
                [] ->
                    {reply, From, {error, failure}}
            end;
        %% Switch to user without password
        {call, From, {switch_user, Username, MacAddress} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:match_object(?USER_DB, #user{name = Username, _ = '_'}) of
                %% User without password exists
                [#user{password_hash = not_set} = User] ->
                    UpdatedUser = User#user{session_id = session_id(),
                                            mac_address = MacAddress,
                                            updated = db:seconds_since_epoch()},
                    ok = dets:insert(?USER_DB, UpdatedUser),
                    {reply, From, {ok, {false, UpdatedUser}}};
                %% User *with* password exists
                [_] ->
                    {reply, From, {error, failure}};
                %% Create new user without password
                [] ->
                    User = #user{id = db_serv:get_user_id(),
                                 name = Username,
                                 session_id = session_id(),
                                 mac_address = MacAddress,
                                 updated = db:seconds_since_epoch()},
                    ok = dets:insert(?USER_DB, User),
                    {reply, From, {ok, {true, User}}}
            end;
        %% Switch to user *with* password
        {call, From, {switch_user, Username, MacAddress, PasswordSalt, PasswordHash} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:match_object(?USER_DB, #user{name = Username, _ = '_'}) of
                %% User without password exists
                [#user{password_hash = not_set} = User] ->
                    UpdatedUser = User#user{session_id = session_id(),
                                            mac_address = MacAddress,
                                            password_salt = PasswordSalt,
                                            password_hash = PasswordHash,
                                            updated = db:seconds_since_epoch()},
                    ok = dets:insert(?USER_DB, User),
                    {reply, From, {ok, {false, UpdatedUser}}};
                %% User *with* password exists
                [User] ->
                    UpdatedUser = User#user{session_id = session_id(),
                                            mac_address = MacAddress,
                                            updated = db:seconds_since_epoch()},
                    ok = dets:insert(?USER_DB, UpdatedUser),
                    {reply, From, {ok, {false, UpdatedUser}}};
                %% Create new user *with* password
                [] ->
                    User = #user{id = db_serv:get_user_id(),
                                 name = Username,
                                 session_id = session_id(),
                                 mac_address = MacAddress,
                                 password_salt = PasswordSalt,
                                 password_hash = PasswordHash,
                                 updated = db:seconds_since_epoch()},
                    ok = dets:insert(?USER_DB, User),
                    {reply, From, {ok, {true, User}}}
            end;
        {call, From, {change_password, Username, MacAddress, PasswordSalt, PasswordHash} = Call} ->
            ?log_debug("Call: ~p", [Call]),
            case dets:match_object(?USER_DB, #user{name = Username, _ = '_'}) of
                [User] ->
                    UpdatedUser = User#user{mac_address = MacAddress,
                                            password_salt = PasswordSalt,
                                            password_hash = PasswordHash},
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
            ok = close_db(),
            exit(Reason);
        {system, From, Request} ->
            ?log_debug("System: ~p", [Request]),
            {system, From, Request};
        UnknownMessage ->
            ?log_error("Unknown message: ~p", [UnknownMessage]),
            noreply
    end.

open_db() ->
    {ok, _} = dets:open_file(?USER_DB, [{file, ?USER_FILE_PATH}, {keypos, #user.id}]),
    case dets:info(?USER_DB, no_keys) of
        0 ->
            ?log_info("Creating admin user"),
            AdminUser = #user{id = db_serv:get_user_id(),
                              name = <<"admin">>,
                              session_id = session_id(),
                              updated = db:seconds_since_epoch()},
            ?ADMIN_USER_ID = AdminUser#user.id, % Assertion
            dets:insert(?USER_DB, AdminUser);
        _ ->
            ok
    end.

close_db() ->
    _ = dets:close(?USER_DB),
    ok.

get_recipients(_IgnoredUsernames, _Query, 0, _UserId) ->
    [];
get_recipients(_IgnoredUsernames, _Query, _N, '$end_of_table') ->
    [];
get_recipients(IgnoredUsernames, Query, N, UserId) ->
    [#user{name = Username}] = dets:lookup(?USER_DB, UserId),
    case lists:member(Username, IgnoredUsernames) of
        false ->
            case binary:match(Username, Query) of
                nomatch ->
                    get_recipients(IgnoredUsernames, Query, N, dets:next(?USER_DB, UserId));
                _ ->
                    [#{username => Username, user_id => UserId, ignored => false}|
                     get_recipients(IgnoredUsernames, Query, N - 1, dets:next(?USER_DB, UserId))]
            end;
        true ->
            get_recipients(IgnoredUsernames, Query, N, dets:next(?USER_DB, UserId))
    end.

%%
%% Generate username
%%

generate_username(WordList) ->
    Username = ?l2b([random_word(WordList), random_word(WordList)]),
    case string:length(Username) of
        N when N > ?MAX_USERNAME_LENGTH ->
            generate_username(WordList);
        _ ->
            case dets:match_object(?USER_DB, #user{name = Username, _ = '_'}) of
                [] ->
                    Username;
                _ ->
                    generate_username(WordList)
            end
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
                    {true, ?l2b([string:to_upper(hd(StrippedWord))|tl(StrippedWord)])}
            end;
        _ ->
            false
    end.

%%
%% Utilities
%%

session_id() ->
    crypto:strong_rand_bytes(?SESSION_ID_SIZE).
