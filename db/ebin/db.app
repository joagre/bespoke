%% -*- erlang -*-
{application, db,
 [{description,"Bespoke database"},
  {vsn,""},
  {modules, [db_app,db_serv,db_sup]},
  {registered, [db_serv]},
  {mod, {db_app, []}},
  {applications, [kernel, stdlib, apptools, rester]}]}.
