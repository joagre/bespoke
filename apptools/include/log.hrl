-ifndef(LOG_HRL).
-define(LOG_HRL, true).


-define(log_debug(Format),
        io:format("**** DEBUG ~w:~w:~w:" ++ Format ++ "\n",
                  [?MODULE, ?LINE, ?FUNCTION_NAME])).

-define(log_debug(Format, Args),
        io:format("**** DEBUG ~w:~w:~w:" ++ Format ++ "\n",
                  [?MODULE, ?LINE, ?FUNCTION_NAME|Args])).

-define(log_info(Format),
        io:format("**** INFO ~w:~w:~w:" ++ Format ++ "\n",
                  [?MODULE, ?LINE, ?FUNCTION_NAME])).

-define(log_info(Format, Args),
        io:format("**** INFO ~w:~w:~w:" ++ Format ++ "\n",
                  [?MODULE, ?LINE, ?FUNCTION_NAME|Args])).

-define(log_error(Format),
        io:format("**** ERROR ~w:~w:~w:" ++ Format ++ "\n",
                  [?MODULE, ?LINE, ?FUNCTION_NAME])).

-define(log_error(Format, Args),
        io:format("**** ERROR ~w:~w:~w:" ++ Format ++ "\n",
                  [?MODULE, ?LINE, ?FUNCTION_NAME|Args])).

-endif.
