%% -*- erlang -*-
{application, main,
 [{description,"Main application"},
  {vsn,""},
  {modules, [main]},
  {registered, []},
  {mod, {db_app, []}},
  {applications, [kernel, stdlib, apptools, rester, db]}]}.
