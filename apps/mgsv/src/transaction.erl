-module(transaction).

-include("common.hrl").
-include("payapp.hrl").


-export([create_transactiontable/0, to_proplist/1, add/2, get/1, reconstruct/1,
        from_proplist/1, delete/1]).

create_transactiontable() ->
    Res = mnesia:create_table( transaction,
                               [ {type, set},
                                 {disc_copies, [node()]},
                                 {index, [paid_by, paid_by_username,
                                          paid_for,
                                          paid_for_username]},
                                 {attributes, record_info(fields, transaction)}]),
    lager:info("Trying to create transaction table with result ~p", [Res]).

add(T = #transaction{}, ReqBy = #user{}) ->
    UT = T#transaction{ edit_details = common:mod_edit_details(T#transaction.edit_details, ReqBy)},
    mnesia:dirty_write(UT);
add(T, ReqBy = #user{}) when is_list(T)->
    add(from_proplist(T), ReqBy);
add(_, no_such_user) ->
    lager:info("NOSUCHUSERERROR in add~n",[]),
    nok.


get(User = #user{}) ->
    transaction:get({paid_id,User#user.internal_uid});
get({User = #user{}, User2 = #user{}}) ->
    transaction:get([{paid_id, User#user.internal_uid},
                     {paid_id, User2#user.internal_uid}]);
get({username, Username}) ->
    mnesia:dirty_index_read(transaction, Username, #transaction.paid_by_username) ++
        mnesia:dirty_index_read(transaction, Username, #transaction.paid_for_username);
get({transaction_id, Id}) ->
    mnesia:dirty_read(transaction, Id);
get({paid_id, Id}) ->
    mnesia:dirty_index_read(transaction, Id, #transaction.paid_by) ++
        mnesia:dirty_index_read(transaction, Id, #transaction.paid_for);
get([T1,T2]) ->
    Ts1 = transaction:get(T1),
    Ts2 = transaction:get(T2),
    Ts3 = Ts1 -- Ts2,
    Ts1 -- Ts3.


delete(T = #transaction{}) ->
    mnesia:dirty_delete(transaction, T#transaction.transaction_id).

to_proplist(Transaction) ->
    [{?UUID, Transaction#transaction.transaction_id},
     {?UID1, Transaction#transaction.paid_by_username},
     {?UID2, Transaction#transaction.paid_for_username},
     {?AMOUNT, Transaction#transaction.amount},
     {?REASON, Transaction#transaction.reason},
     {?TIMESTAMP, Transaction#transaction.timestamp},
     {?SERVER_TIMESTAMP, Transaction#transaction.server_timestamp},
     {?CURRENCY, Transaction#transaction.currency}] ++
     org_transaction_to_proplist(Transaction#transaction.org_transaction).

org_transaction_to_proplist(#org_transaction{currency = undefined}) ->
    [];
org_transaction_to_proplist(#org_transaction{amount = undefined}) ->
    [];
org_transaction_to_proplist(OrgDebt) ->
    [{?ORG_DEBT,
     [{?CURRENCY, OrgDebt#org_transaction.currency},
      {?AMOUNT, OrgDebt#org_transaction.amount}]}].

from_proplist(List) ->
    %% This should be removed in later versions
    UList = case {proplists:get_value(?UID1, List),
                  proplists:get_value(?USER1, List)} of
                {undefined, undefined} ->
                    List;
                {undefined, DisplayName} ->
                    Uid = common:binary_uuid(),
                    C = proplists:get_value(?CURRENCY, List, <<"SEK">>),
                    users:add(Uid, Uid, DisplayName, ?LOCAL_USER, C, Uid),
                    List ++ [{?UID1, Uid}];
                _ -> List
            end,
    UList2 = case {proplists:get_value(?UID2, List),
                   proplists:get_value(?USER2, List)} of
                {undefined, undefined} ->
                    UList;
                {undefined, DisplayName2} ->
                    Uid2 = common:binary_uuid(),
                    C2 = proplists:get_value(?CURRENCY, List, <<"SEK">>),
                    users:add(Uid2, Uid2, DisplayName2, ?LOCAL_USER, C2, Uid2),
                    UList ++ [{?UID2, Uid2}];
                _ -> UList
            end,
    check_for_missing_fields(
      from_proplist(#transaction{}, UList2)).

from_proplist(Transaction, []) ->
    Transaction;
from_proplist(T, [{?UID1, Uid1}|Rest]) ->
    User1 = users:get(Uid1),
    case User1 of
        no_such_user -> no_such_user;
        _ ->
            from_proplist(T#transaction{paid_by = User1#user.internal_uid,
                                        paid_by_username = User1#user.username},
                          Rest)
    end;
from_proplist(T, [{?UID2, Uid2}|Rest]) ->
    User2 = users:get(Uid2),
    case User2 of
        no_such_user -> no_such_user;
        _ ->
            from_proplist(T#transaction{paid_for = User2#user.internal_uid,
                                        paid_for_username = User2#user.username},
                          Rest)
    end;
from_proplist(T, [{?AMOUNT, Amount}|Rest]) ->
    from_proplist(T#transaction{amount = Amount}, Rest);
from_proplist(T, [{?REASON, Reason}|Rest]) ->
    from_proplist(T#transaction{reason = Reason}, Rest);
from_proplist(T, [{?TIMESTAMP, Timestamp}|Rest]) ->
    from_proplist(T#transaction{timestamp = Timestamp},Rest);
from_proplist(T, [{?CURRENCY, Currency}|Rest]) ->
    from_proplist(T#transaction{currency = Currency},Rest);
from_proplist(T, [{?UUID, UUid}|Rest]) ->
    from_proplist(T#transaction{transaction_id = UUid},Rest);
from_proplist(T, [{?SERVER_TIMESTAMP, ServerTimestamp}|Rest]) ->
    from_proplist(T#transaction{server_timestamp = ServerTimestamp}, Rest);
from_proplist(T, [{?ORG_DEBT, OrgDebt}|Rest]) ->
    from_proplist(T#transaction{org_transaction = org_transaction_from_proplist(T#transaction.org_transaction, OrgDebt)}, Rest);
from_proplist(T, [Any|Rest]) ->
    lager:info("unsupported transaction variable ~p~n",[Any]),
    from_proplist(T,Rest).

org_transaction_from_proplist(O, []) ->
    O;
org_transaction_from_proplist(O, [{?CURRENCY, Currency}|Rest]) ->
    org_transaction_from_proplist(O#org_transaction{currency = Currency}, Rest);
org_transaction_from_proplist(O, [{?AMOUNT, Amount}|Rest]) ->
    org_transaction_from_proplist(O#org_transaction{ amount = Amount}, Rest);
org_transaction_from_proplist(T, [Any|Rest]) ->
    lager:info("unsupported transaction.org_transaction variable ~p~n",[Any]),
    org_transaction_from_proplist(T,Rest).

check_for_missing_fields(#transaction{paid_by = undefined}) ->
    {error, missing_paid_by};
check_for_missing_fields(#transaction{paid_by_username = undefined}) ->
    {error, missing_paid_by};
check_for_missing_fields(#transaction{paid_for = undefined}) ->
    {error, missing_paid_for};
check_for_missing_fields(#transaction{paid_for_username = undefined}) ->
    {error, missing_paid_for_username};
check_for_missing_fields(#transaction{amount = undefined}) ->
    {error, missing_amount};
%% debt to self not allowed:
check_for_missing_fields(#transaction{paid_by_username = P, paid_for_username = P}) ->
    {error, missing_amount};
check_for_missing_fields(Transaction = #transaction{timestamp = undefined,
                                                   server_timestamp = S}) ->
    check_for_missing_fields(Transaction#transaction{timestamp = S});
check_for_missing_fields(Transaction = #transaction{currency = undefined}) ->
    check_for_missing_fields(Transaction#transaction{currency = <<"SEK">>});
check_for_missing_fields(T = #transaction{}) ->
    T;
check_for_missing_fields(V) ->
    {error, V}.

reconstruct(DBName) ->
    dets:traverse(DBName,
                  fun({_DebtId, PropList}) ->
                          case from_proplist(PropList) of
                              #transaction{} = T ->
                                  ReqBy = users:get({internal_uid, T#transaction.paid_by}),
                                  ok = case ReqBy of
                                      no_such_user ->
                                               lager:info("error do not find user ~p in transaction ~n~p~n", [T#transaction.paid_by, T]);
                                           _ ->
                                               add(T, ReqBy)
                                       end;
                              E -> lager:info("error ~p when adding ~p~n",[E,PropList])
                          end,
                          continue
                  end).
