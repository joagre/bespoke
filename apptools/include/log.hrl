-ifndef(LOG_HRL).
-define(LOG_HRL, true).

-define(log_info(StringOrReport),
        case is_map(StringOrReport) of
            true ->
                logger:info(
                  maps:merge(#{module => ?MODULE,
                               line => ?LINE,
                               function => ?FUNCTION_NAME}, StringOrReport));
            false ->
                logger:info(StringOrReport)
        end).

-define(log_info(FormatOrFun, Args),
        case is_map(Args) of
            true ->
                logger:info(FormatOrFun,
                            maps:merge(#{module => ?MODULE,
                                         line => ?LINE,
                                         function => ?FUNCTION_NAME}, Args));
            false ->
                logger:info(FormatOrFun, Args,
                            #{module => ?MODULE,
                              line => ?LINE,
                              function => ?FUNCTION_NAME})
        end).

-define(log_info(FormatOrFun, Args, Metadaya),
        logger:info(FormatOrFun, Args,
                    maps:merge(#{module => ?MODULE,
                                 line => ?LINE,
                                 function => ?FUNCTION_NAME}, MetaData))).

-define(log_debug(StringOrReport),
        case is_map(StringOrReport) of
            true ->
                logger:debug(
                  maps:merge(#{module => ?MODULE,
                               line => ?LINE,
                               function => ?FUNCTION_NAME}, StringOrReport));
            false ->
                logger:debug(StringOrReport)
        end).

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
                                  function => ?FUNCTION_NAME}, MetaData))).

-define(log_error(StringOrReport),
        case is_map(StringOrReport) of
            true ->
                logger:error(
                  maps:merge(#{module => ?MODULE,
                               line => ?LINE,
                               function => ?FUNCTION_NAME}, StringOrReport));
            false ->
                logger:error(StringOrReport)
        end).

-define(log_error(FormatOrFun, Args),
        case is_map(Args) of
            true ->
                logger:error(FormatOrFun,
                             maps:merge(#{module => ?MODULE,
                                          line => ?LINE,
                                          function => ?FUNCTION_NAME}, Args));
            false ->
                logger:error(FormatOrFun, Args,
                             #{module => ?MODULE,
                               line => ?LINE,
                               function => ?FUNCTION_NAME})
        end).

-define(log_error(FormatOrFun, Args, Metafaya),
        logger:error(FormatOrFun,
                     Args,
                     maps:merge(#{module => ?MODULE,
                                  line => ?LINE,
                                  function => ?FUNCTION_NAME}, MetaData))).


-endif.
