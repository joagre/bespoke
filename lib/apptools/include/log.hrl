% -*- fill-column: 100; -*-

-ifndef(LOG_HRL).
-define(LOG_HRL, true).

-define(log_info(Format),
        logger:info(Format, [], #{module => ?MODULE,
                                  line => ?LINE,
                                  function => ?FUNCTION_NAME,
                                  arity => ?FUNCTION_ARITY})).

-define(log_info(Format, Args),
        logger:info(Format, Args, #{module => ?MODULE,
                                    line => ?LINE,
                                    function => ?FUNCTION_NAME,
                                    arity => ?FUNCTION_ARITY})).

-define(log_debug(Format),
        logger:debug(Format, [], #{module => ?MODULE,
                                   line => ?LINE,
                                   function => ?FUNCTION_NAME,
                                   arity => ?FUNCTION_ARITY})).

-define(log_debug(Format, Args),
        logger:debug(Format, Args, #{module => ?MODULE,
                                     line => ?LINE,
                                     function => ?FUNCTION_NAME,
                                     arity => ?FUNCTION_ARITY})).

-define(log_error(Format),
        logger:error(Format, [], #{module => ?MODULE,
                                   line => ?LINE,
                                   function => ?FUNCTION_NAME,
                                   arity => ?FUNCTION_ARITY})).

-define(log_error(Format, Args),
        logger:error(Format, Args, #{module => ?MODULE,
                                     line => ?LINE,
                                     function => ?FUNCTION_NAME,
                                     arity => ?FUNCTION_ARITY})).

-endif.
