%% -*- erlang -*-
{application, db,
 [{description,"Bespoke database"},
  {vsn,"0464647"},
  {modules, [db_app,db_serv,db_sup,db_tools,db_user_serv]},
  {registered, [db_serv, db_user_serv]},
  {mod, {db_app, []}},
  {applications, [kernel, stdlib, apptools, rester]}]}.
