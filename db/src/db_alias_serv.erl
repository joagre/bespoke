-module(db_alias_serv).
-export([start_link/0, stop/0]).
-export([new_name/0, add_alias/2, mac_address_to_name/1,
         name_to_mac_address/1]).
-export([message_handler/1]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include("db.hrl").

-define(ALIAS_DB_FILENAME, "aliases.db").
-define(ALIAS_DB, aliases).
-define(WORD_LIST_PATH, "/usr/share/dict/words").

-type name() :: binary().

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
%% Exported: new_name
%%

-spec new_name() -> name().

new_name() ->
    serv:call(?MODULE, new_name).

%%
%% Exported: add_alias
%%

-spec add_alias(name(), db_dnsmasq:mac_address()) ->
          ok | {error, already_taken}.

add_alias(Name, MacAddress) ->
    serv:call(?MODULE, {add_alias, Name, MacAddress}).

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
        {call, From, new_name} ->
            ?log_debug("Call: ~p", [new_name]),
            {reply, From, generate_name(S#state.word_list)};
        {call, From, {add_alias, Name, MacAddress}} ->
            ?log_debug("Call: ~p", [{add_alias, Name, MacAddress}]),
            case dets:lookup(?ALIAS_DB, Name) of
                [] ->
                    ok = dets:insert(
                           ?ALIAS_DB,
                           #alias{name = Name, mac_address = MacAddress}),
                    {reply, From, ok};
                _ ->
                    {reply, From, {error, already_taken}}
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
%% Aliases
%%

generate_name(WordList) ->
    Name = string:concat(random_word(WordList), random_word(WordList)),
    case dets:lookup(?ALIAS_DB, Name) of
        [] ->
            Name;
        _ ->
            generate_name(WordList)
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
                    %% Capitalize the first letter
                    CapitalizedWord = string:concat(
                                        string:to_upper(string:substr(StrippedWord, 1, 1)),
                                        string:substr(StrippedWord, 2)),
                    {true, ?l2b(CapitalizedWord)}
            end;
        _ ->
            false
    end.
