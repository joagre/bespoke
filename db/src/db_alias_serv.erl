-module(db_alias_serv).
-export([start_link/0, stop/0]).
-export([get_name/1, authenticate/3, mac_address_to_name/1, name_to_mac_address/1]).
-export([message_handler/1]).
-export_type([name/0, user_id/0, pwhash/0, session_id/0, password/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include("db.hrl").

-define(ALIAS_DB_FILENAME, "aliases.db").
-define(ALIAS_DB, aliases).
-define(WORD_LIST_PATH, "/usr/share/dict/words").
-define(SESSION_ID_SIZE, 16).

-type name() :: binary().
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
%% Exported: get_alias
%%

-spec get_name(db_dnsmasq:mac_address()) -> name().

get_name(MacAddress) ->
    serv:call(?MODULE, {get_name, MacAddress}).

%%
%% Exported: authenticate
%%

-spec authenticate(name(), password(), db_dnsmasq:mac_address()) ->
          {ok, session_id()} | {error, failure}.

authenticate(Name, Password, MacAddress) ->
    serv:call(?MODULE, {authenticate, Name, Password, MacAddress}).

%%
%% Exported: mac_address_to_name
%%

-spec mac_address_to_name(db_dnsmasq:mac_address()) ->
          {ok, name()} | {error, not_found}.

mac_address_to_name(MacAddress) ->
    serv:call(?MODULE, {mac_address_to_name, MacAddress}).

%%
%% Exported: name_to_mac_address
%%

-spec name_to_mac_address(name()) ->
          {ok, db_dnsmasq:mac_address()} | {error, not_found}.

name_to_mac_address(Name) ->
    serv:call(?MODULE, {name_to_mac_address, Name}).

%%
%% Server
%%

init(Parent) ->
    {ok, ?ALIAS_DB} =
        dets:open_file(
          ?ALIAS_DB,
          [{file, filename:join(code:priv_dir(db), ?ALIAS_DB_FILENAME)},
           {keypos, #alias.name}]),
    ?log_info("Database alias server has been started"),
    {ok, #state{parent = Parent,
                word_list = init_word_list()}}.

message_handler(S) ->
    receive
        {call, From, stop = Call} ->
            ?log_debug("Call: ~p", [Call]),
            ok = dets:close(?ALIAS_DB),
            {reply, From, ok};
        {call, From, {get_name, MacAddress}} ->
            ?log_debug("Call: ~p", [{get_name, MacAddress}]),
            case dets:match_object(?ALIAS_DB,
                                   #alias{mac_address = MacAddress, _ = '_'}) of
                [Alias|_] ->
                    {reply, From, Alias#alias.name};
                [] ->
                    {reply, From, generate_name(S#state.word_list)}
            end;
        {call, From, {authenticate, Name, Password, MacAddress}} ->
            ?log_debug("Call: ~p", [{authenticate, Name, Password, MacAddress}]),
            case dets:lookup(?ALIAS_DB, Name) of
                [Alias] ->
                    ok;
                [] ->
                    Alias = #alias{name = Name,
                                   user_id = db_serv:allocate_user_id(),
                                   mac_address = MacAddress},
                    ok = dets:insert(?ALIAS_DB, Alias)
            end,
            case do_authenticate(Alias, Password) of
                {ok, SessionId} ->
                    {reply, From, {ok, SessionId}};
                {error, Reason} ->
                    {reply, From, {error, Reason}}
            end;
        {call, From, {mac_address_to_name, MacAddress}} ->
            ?log_debug("Call: ~p", [{mac_address_to_name, MacAddress}]),
            case dets:match_object(?ALIAS_DB,
                                   #alias{mac_address = MacAddress, _ = '_'}) of
                [Alias] ->
                    {reply, From, {ok, Alias#alias.name}};
                [] ->
                    {reply, From, {error, not_found}}
            end;
        {call, From, {name_to_mac_address, Name}} ->
            ?log_debug("Call: ~p", [{name_to_mac_address, Name}]),
            case dets:lookup(?ALIAS_DB, Name) of
                [#alias{mac_address = MacAddress}] ->
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
%% Call: get_name
%%

generate_name(WordList) ->
    Name = ?l2b([random_word(WordList), random_word(WordList)]),
    case dets:lookup(?ALIAS_DB, Name) of
        [] ->
            Name;
        _ ->
            generate_name(WordList)
    end.

random_word(Words) ->
    Index = enacl:randombytes_uniform(length(Words) + 1),
    lists:nth(Index, Words).

%%
%% Call: authenticate
%%

%% Proceed without password
do_authenticate(#alias{pwhash = not_set} = Alias, <<>>) ->
    SessionId = session_id(),
    ok = dets:insert(?ALIAS_DB, Alias#alias{session_id = SessionId}),
    {ok, SessionId};
%% Set a password
do_authenticate(#alias{pwhash = not_set} = Alias, Password) ->
    Pwhash = enacl:pwhash_str(Password, interactive, interactive),
    SessionId = session_id(),
    ok = dets:insert(?ALIAS_DB, Alias#alias{pwhash = Pwhash,
                                            session_id = SessionId}),
    {ok, SessionId};
%% Verify password
do_authenticate(#alias{pwhash = Pwhash} = Alias, Password) ->
    case enacl:pwhash_str_verify(Pwhash, Password) of
        true ->
            SessionId = session_id(),
            ok = dets:insert(?ALIAS_DB, Alias#alias{session_id = SessionId}),
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
