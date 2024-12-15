%% -*- erlang -*-
{application, main,
 [{description,"Main application"},
  {vsn,"a7f4c8f"},
  {modules, [main]},
  {registered, []},
  {mod, {db_app, []}},
  {applications, [kernel, stdlib, apptools, rester, db]}]}.
