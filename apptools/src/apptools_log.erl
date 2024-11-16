-module(apptools_log).
-export([info/4, info/5,
         debug/4,
         error/4, error/5]).

%%
%% Exported: log_info
%%

info(Map, Module, Line, FunctionName) when is_map(Map) ->
    logger:info(
      maps:merge(#{module => Module,
                   line => Line,
                   function => FunctionName}, Map));
info(StringOrReport, _, _, _) ->
    logger:info(StringOrReport).

%%
%% Exported: info
%%

info(FormatOrFun, Map, Module, Line, FunctionName) when is_map(Map) ->
    logger:info(FormatOrFun,
                maps:merge(#{module => Module,
                             line => Line,
                             function => FunctionName}, Map));
info(FormatOrFun, Args, Module, Line, FunctionName) ->
    logger:info(FormatOrFun, Args,
                #{module => Module,
                  line => Line,
                  function => FunctionName}).

%%
%% Exported: debug
%%

debug(Map, Module, Line, FunctionName) when is_map(Map) ->
    logger:debug(
      maps:merge(#{module => Module,
                   line => Line,
                   function => FunctionName}, Map));
debug(StringOrReport, _, _, _) ->
    logger:debug(StringOrReport).

%%
%% Exported: error
%%

error(Map, Module, Line, FunctionName) when is_map(Map) ->
    logger:error(
      maps:merge(#{module => Module,
                   line => Line,
                   function => FunctionName}, Map));
error(StringOrReport, _, _, _) ->
    logger:error(StringOrReport).

error(_FormatOrFun, Map, Module, Line, FunctionName) when is_map(Map) ->
    maps:merge(#{module => Module,
                 line => Line,
                 function => FunctionName}, Map);
error(FormatOrFun, Args, Module, Line, FunctionName) ->
    logger:error(FormatOrFun, Args,
                 #{module => Module,
                   line => Line,
                   function => FunctionName}).
