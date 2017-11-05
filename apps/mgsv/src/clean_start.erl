-module(clean_start).

-export([setup_db/0]).

setup_db() ->
    application:stop(mnesia),
    mnesia:create_schema([node()]),
    application:start(mnesia),
    debt:create_debttable(),
    users:create_usertable(),
    users:create_mappingtable(),
    transaction:create_transactiontable().
