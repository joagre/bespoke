% -*- fill-column: 100; -*-

-ifndef(DB_META_DB_HRL).
-define(DB_META_DB_HRL, true).

-include("../include/db.hrl").

-define(META_FILE_PATH, filename:join(?BESPOKE_DB_DIR, "meta.db")).
-define(META_DB, meta_db).

-endif.
