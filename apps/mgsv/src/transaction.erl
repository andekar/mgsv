-module(transaction).

-include("common.hrl").
-include("payapp.hrl").


-export([create_transactiontable/0, to_proplist/2, add/2, get/1,
        from_proplist/2, delete/1, change_internal_uid/2]).

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
    {atomic,Res} = mnesia:transaction(fun() ->
                              mnesia:write(UT) end),
    Res;
add(_, no_such_user) ->
    lager:info("NOSUCHUSERERROR in add~n",[]),
    nok.


get(User = #user{}) ->
    transaction:get({paid_id,User#user.internal_uid});
get({User = #user{}, User2 = #user{}}) ->
    transaction:get([{paid_id, User#user.internal_uid},
                     {paid_id, User2#user.internal_uid}]);
get({username, Username}) ->
    {atomic,Res} = mnesia:transaction(fun() ->
                              mnesia:index_read(transaction, Username, #transaction.paid_by_username) ++
                                  mnesia:index_read(transaction, Username, #transaction.paid_for_username) end),
    Res;
get({transaction_id, Id}) ->
    {atomic,Res} = mnesia:transaction(fun() ->
                              mnesia:read(transaction, Id) end),
    Res;
get({paid_id, Id}) ->
    {atomic,Res} = mnesia:transaction(fun() ->
                                              mnesia:index_read(transaction, Id, #transaction.paid_by) ++
                                                  mnesia:index_read(transaction, Id, #transaction.paid_for) end),
    Res;
get([T1,T2]) ->
    Ts1 = transaction:get(T1),
    Ts2 = transaction:get(T2),
    Ts3 = Ts1 -- Ts2,
    Ts1 -- Ts3.

change_internal_uid(OldUser, NewUser) ->
    Transactions = transaction:get(OldUser),
    NewUuid = NewUser#user.internal_uid,
    lists:foreach(fun(T)->
                          OldUuid = OldUser#user.internal_uid,

                          case T#transaction.paid_by of
                              OldUuid ->
                                  transaction:add(T#transaction{ paid_by = NewUuid }, NewUser);
                              _ ->
                                  transaction:add(T#transaction{ paid_for = NewUuid }, NewUser)
                          end
                  end, Transactions).

delete(T = #transaction{}) ->
    {atomic,Res} = mnesia:transaction(fun() ->
                              mnesia:delete({transaction, T#transaction.transaction_id}) end),
    Res.

to_proplist(User, Userdata) ->
    case Userdata#user_data.protocol of
        "0.36" ->
            to_proplist_36(User);
        "0.37" ->
            to_proplist_36(User);
        _ ->
            to_proplist_old(User)
    end.

to_proplist_36(User) ->
    Fields = record_info(fields, transaction),
    DFields = record_info(fields, edit_details),
    OFields = record_info(fields, org_transaction),
    [_|Vals] = tuple_to_list(User),
    Ret =
        lists:zipwith(fun (edit_details, DY) ->
                              [_|DVals] = tuple_to_list(DY),
                              {<<"edit_details">>,
                               ?JSONSTRUCT(lists:zipwith(fun(A,B) ->
                                                                 {atom_to_binary(A, utf8),B} end,
                                                         DFields, DVals))};
                          (org_transaction, DY) ->
                              [_|DVals] = tuple_to_list(DY),
                              {<<"org_transaction">>,
                               ?JSONSTRUCT(lists:zipwith(fun(A,B) ->
                                                                 {atom_to_binary(A, utf8),B} end,
                                                         OFields, DVals))};
                          (X, Y) ->
                              {atom_to_binary(X, utf8),Y} end,
                      Fields,
                      Vals),
    lists:foldl(fun(ToRemove, State) ->
                        proplists:delete(ToRemove, State) end,
                Ret, [<<"paid_by_username">>,
                      <<"paid_for_username">>]
               ).

to_proplist_old(Transaction) ->
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

from_proplist({?TRANSACTION, List}, UD) ->
    from_proplist(List,UD);
from_proplist(List,Userdata) ->
    T = case Userdata#user_data.protocol of
            "0.36" ->
                from_proplist36(List, Userdata#user_data.user);
            "0.37" ->
                from_proplist36(List, Userdata#user_data.user);
            _ ->
                from_proplist_old(List, Userdata#user_data.user)
        end,
    validate(T).

validate(#transaction{} =T) ->
    T; %%TODO
validate(Other) ->
    Other.

from_proplist36(List, #user{} = ReqBy) ->
    check_for_missing_fields(
      from_proplist36(#transaction{
                         edit_details =
                             common:mod_edit_details(
                               #edit_details{},
                               ReqBy)
                        }, List));

from_proplist36(Transaction, []) ->
    OrgTrans = Transaction#transaction.org_transaction,
    case OrgTrans#org_transaction.amount of
        undefined ->
            Amount = Transaction#transaction.amount,
            Currency = Transaction#transaction.currency,
            UOrgTrans = OrgTrans#org_transaction{amount = Amount,
                                                 currency = Currency},
            Transaction#transaction{org_transaction = UOrgTrans};
        _ ->
            Transaction
    end;
from_proplist36(T, [{?PAID_BY, Uid1}|Rest]) ->
    User1 = users:get(Uid1),
    case User1 of
        no_such_user -> no_such_user;
        _ ->
            from_proplist36(T#transaction{paid_by = User1#user.internal_uid,
                                          paid_by_username = User1#user.username},
                          Rest)
    end;
from_proplist36(T, [{?PAID_FOR, Uid2}|Rest]) ->
    User2 = users:get(Uid2),
    case User2 of
        no_such_user -> no_such_user;
        _ ->
            from_proplist36(T#transaction{paid_for = User2#user.internal_uid,
                                          paid_for_username = User2#user.username},
                          Rest)
    end;
from_proplist36(T, [{?AMOUNT, Amount}|Rest]) ->
    from_proplist36(T#transaction{amount = Amount}, Rest);
from_proplist36(T, [{?REASON, Reason}|Rest]) ->
    from_proplist36(T#transaction{reason = Reason}, Rest);
from_proplist36(T, [{?TIMESTAMP, Timestamp}|Rest]) ->
    from_proplist36(T#transaction{timestamp = Timestamp},Rest);
from_proplist36(T, [{?CURRENCY, Currency}|Rest]) ->
    from_proplist36(T#transaction{currency = Currency},Rest);
from_proplist36(T, [{?UUID, UUid}|Rest]) ->
    from_proplist36(T#transaction{transaction_id = UUid},Rest);
from_proplist36(T, [{?SERVER_TIMESTAMP, ServerTimestamp}|Rest]) ->
    from_proplist36(T#transaction{server_timestamp = ServerTimestamp}, Rest);
from_proplist36(T, [{?ORG_DEBT, OrgDebt}|Rest]) ->
    from_proplist36(T#transaction{org_transaction = org_transaction_from_proplist(T#transaction.org_transaction, OrgDebt)}, Rest);
from_proplist36(T, [{?ORG_TRANSACTION, OrgDebt}|Rest]) ->
    from_proplist36(T#transaction{org_transaction = org_transaction_from_proplist(T#transaction.org_transaction, OrgDebt)}, Rest);
from_proplist36(T, [{?ECHO_UUID, _}|Rest]) ->
    from_proplist36(T,Rest);
from_proplist36(_T, [Any|_Rest]) ->
    lager:info("unsupported transaction variable ~p~n",[Any]),
    unsupported_variable.

from_proplist_old(List, #user{} = ReqBy) ->
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
      from_proplist_old(#transaction{
                           edit_details =
                               common:mod_edit_details(
                                 #edit_details{},
                                 ReqBy)}, UList2));

from_proplist_old(Transaction, []) ->
    OrgTrans = Transaction#transaction.org_transaction,
    case OrgTrans#org_transaction.amount of
        undefined ->
            Amount = Transaction#transaction.amount,
            Currency = Transaction#transaction.currency,
            UOrgTrans = OrgTrans#org_transaction{amount = Amount,
                                                 currency = Currency},
            Transaction#transaction{org_transaction = UOrgTrans};
        _ ->
            Transaction
    end;
from_proplist_old(T, [{?UID1, Uid1}|Rest]) ->
    User1 = users:get(Uid1),
    case User1 of
        no_such_user -> no_such_user;
        _ ->
            from_proplist_old(T#transaction{paid_by = User1#user.internal_uid,
                                        paid_by_username = User1#user.username},
                          Rest)
    end;
from_proplist_old(T, [{?UID2, Uid2}|Rest]) ->
    User2 = users:get(Uid2),
    case User2 of
        no_such_user -> no_such_user;
        _ ->
            from_proplist_old(T#transaction{paid_for = User2#user.internal_uid,
                                        paid_for_username = User2#user.username},
                          Rest)
    end;
from_proplist_old(T, [{?AMOUNT, Amount}|Rest]) ->
    from_proplist_old(T#transaction{amount = Amount}, Rest);
from_proplist_old(T, [{?REASON, Reason}|Rest]) ->
    from_proplist_old(T#transaction{reason = Reason}, Rest);
from_proplist_old(T, [{?TIMESTAMP, Timestamp}|Rest]) ->
    from_proplist_old(T#transaction{timestamp = Timestamp},Rest);
from_proplist_old(T, [{?CURRENCY, Currency}|Rest]) ->
    from_proplist_old(T#transaction{currency = Currency},Rest);
from_proplist_old(T, [{?UUID, UUid}|Rest]) ->
    from_proplist_old(T#transaction{transaction_id = UUid},Rest);
from_proplist_old(T, [{?SERVER_TIMESTAMP, ServerTimestamp}|Rest]) ->
    from_proplist_old(T#transaction{server_timestamp = ServerTimestamp}, Rest);
from_proplist_old(T, [{?ORG_DEBT, OrgDebt}|Rest]) ->
    from_proplist_old(T#transaction{org_transaction = org_transaction_from_proplist(T#transaction.org_transaction, OrgDebt)}, Rest);
from_proplist_old(T, [{?ECHO_UUID, _}|Rest]) ->
    from_proplist_old(T, Rest);
from_proplist_old(T, [Any|Rest]) ->
    lager:info("unsupported transaction variable ~p~n",[Any]),
    from_proplist_old(T,Rest).

org_transaction_from_proplist(O, []) ->
    O;
org_transaction_from_proplist(O, [{?CURRENCY, Currency}|Rest]) ->
    org_transaction_from_proplist(O#org_transaction{currency = Currency}, Rest);
org_transaction_from_proplist(O, [{?AMOUNT, Amount}|Rest]) ->
    org_transaction_from_proplist(O#org_transaction{ amount = Amount}, Rest);
org_transaction_from_proplist(T, [Any|Rest]) ->
    lager:error("unsupported transaction.org_transaction variable ~p~n",[Any]),
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
