-ifndef(LOG_HRL).
-define(LOG_HRL, true).

-define(log_info(StringOrReport),
        apptools_log:info(StringOrReport, ?MODULE, ?LINE, ?FUNCTION_NAME)).


-define(log_info(FormatOrFun, Args),
        apptools_log:info(FormatOrFun, Args, ?MODULE, ?LINE, ?FUNCTION_NAME)).

-define(log_info(FormatOrFun, Args, Metadaya),
        logger:info(FormatOrFun, Args,
                    maps:merge(#{module => ?MODULE,
                                 line => ?LINE,
                                 function => ?FUNCTION_NAME}, MetaData))).

-define(log_debug(StringOrReport),
        apptools_log:debug(StringOrReport, ?MODULE, ?LINE, ?FUNCTION_NAME)).

-define(log_debug(FormatOrFun, Args),
        case is_map(Args) of
            true ->
                logger:debug(FormatOrFun,
                             maps:merge(#{module => ?MODULE,
                                          line => ?LINE,
                                          function => ?FUNCTION_NAME}, Args));
            false ->
                logger:debug(FormatOrFun, Args,
                             #{module => ?MODULE,
                               line => ?LINE,
                               function => ?FUNCTION_NAME})
        end).

-define(log_debug(FormatOrFun, Args, Metafaya),
        logger:debug(FormatOrFun,
                     Args,
                     maps:merge(#{module => ?MODULE,
                                  line => ?LINE,
<                                  function => ?FUNCTION_NAME}, MetaData))).

-define(log_error(StringOrReport),
        apptools_log:error(StringOrReport, ?MODULE, ?LINE, ?FUNCTION_NAME)).

-define(log_error(FormatOrFun, Args),
        apptools_log:error(FormatOrFun, Args, ?MODULE, ?LINE, ?FUNCTION_NAME)).

-define(log_error(FormatOrFun, Args, Metafaya),
        logger:error(FormatOrFun,
                     Args,
                     maps:merge(#{module => ?MODULE,
                                  line => ?LINE,
                                  function => ?FUNCTION_NAME}, MetaData))).


-endif.
