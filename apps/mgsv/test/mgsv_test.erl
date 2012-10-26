-module(mgsv_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/payapp.hrl").
-compile(export_all).

%% helpers

state() ->
    [ {?DEBTS,?DEBTS}
    , {?USERS, ?USERS}
    , {?DEBT_RECORD, ?DEBT_RECORD}
    , {?DEBT_APPROVAL_TRANSACTIONS, ?DEBT_APPROVAL_TRANSACTIONS}].

user_db() ->
    [ {pay_server:binary_uuid(), <<"user1">>}
    , {pay_server:binary_uuid(), <<"user3">>}
    , {pay_server:binary_uuid(), <<"user4">>}
    , {pay_server:binary_uuid(), <<"user5">>}
    , {pay_server:binary_uuid(), <<"user6">>}
    , {pay_server:binary_uuid(), <<"user7">>}].

%% TODO add several debts per user
user_transactions(0) ->
    {[], [], [], []};
user_transactions(Num) ->
    Uuid = pay_server:binary_uuid(),
    User1 = pay_server:binary_uuid(),
    User2 = pay_server:binary_uuid(),
    Amount = Num *10,
    Debt = make_debt(Uuid, User1, User2, Amount),
    Transaction1 = debt_transactions(User1, [Uuid], []),
    Transaction2 = debt_transactions(User2, [Uuid], []),
    { OtherUsers, OtherDebts, OtherApprovalDebt, OtherDebt } = user_transactions(Num - 1),
    { [{User1,<<"user1">>}, {User2, <<"user2">>} |OtherUsers]
    , [ {{User1, User2}, Amount} |OtherDebts]
    , [ Transaction1, Transaction2 |OtherApprovalDebt]
    , [ Debt |OtherDebt] }.


not_in_list_debt() ->
    { pay_server:binary_uuid()
    , {testuid7, testuid8}
    , pay_server:get_timestamp()
    , <<"whatever reason">>
    , 120}.

debt() ->
    make_debt(testuid1, testuid2, 120).

make_debt(User1, User2, Amount) ->
    make_debt(pay_server:binary_uuid(), User1, User2, Amount).

make_debt(Uuid, User1, User2, Amount) ->
    { Uuid
    , {User1, User2}
    , pay_server:get_timestamp()
    , <<"whatever reason">>
    , Amount}.

reversed_debt(Debt) ->
    {Uuid, {P1, P2}, TimeStamp, Reason, Amount} = Debt,
    { Uuid
    , {P2, P1}
    , TimeStamp
    , Reason
    , -1* Amount}.

debt_transactions() ->
    [{testuid1, user_debt_transactions()}].

debt_transactions(Uuid, ApprovedDebts, NonApprovedDebts) ->
    {Uuid, user_debt_transactions(ApprovedDebts, NonApprovedDebts)}.

user_debt_transactions() ->
    [{approved_debts, approved_debts_fun()}, {nonapproved_debts, nonapproved_debts_fun()}].

user_debt_transactions(ApprovedDebts, NonApprovedDebts) ->
    [{?APPROVED_DEBTS, ApprovedDebts}, {?NOT_APPROVED_DEBTS, NonApprovedDebts}].

updated_user_debt_transactions() ->
    [{approved_debts, updated_approved_debts_fun()}, {nonapproved_debts, nonapproved_debts_fun()}].

approved_debts_fun() ->
    [ <<"uid1">>
    , <<"uid2">>
    , <<"uid3">>
    , <<"uid4">>].

new_approved_debts_fun() ->
    [ <<"uid5">>
    , <<"uid6">>
    , <<"uid7">>
    , <<"uid8">>].

updated_approved_debts_fun() ->
    approved_debts_fun() ++ new_approved_debts_fun().

nonapproved_debts_fun() ->
    [ <<"nouid1">>
    , <<"nouid2">>
    , <<"nouid3">>].

updated_debt_transactions() ->
    {testuid1, updated_user_debt_transactions()}.

debts_table() ->
    [ {{testuid1, testuid2}, -128}
    , {{testuid2, testuid3}, 132}
    , {{testuid2, testuid4}, 92}
    , {{testuid1, testuid4}, -2}
    , {{testuid3, testui4}, -39}].

-define(VALID_UID, <<"valid_email@gmail.com">>).
-define(INVALID_UID, <<"invalid_user_id">>).

verify_user_valid_test() ->
    ?assertEqual(pay_server:verify_uid(?VALID_UID), ok).

verify_user_invalid_test() ->
    ?assertEqual(pay_server:verify_uid(?INVALID_UID), invalid).

verify_replace_prop_test() ->
    Props = user_debt_transactions(),
    NewProps = new_approved_debts_fun(),
    Result = pay_server:replace_prop(approved_debts, Props, NewProps),
    ?assertEqual(NewProps, proplists:get_value(approved_debts, Result)),
    ?assertEqual([], proplists:delete(nonapproved_debts,(proplists:delete(approved_debts, Result)))),
    ?assertEqual(proplists:get_value(nonapproved_debts, Props), proplists:get_value(nonapproved_debts, Result)).

verify_lookup_dets_default_test() ->
    ok = meck:new(db_w),
    ok = meck:expect(db_w, lookup, fun(_Name, _Key) -> [] end),

    ?assertEqual([apa, bepa, cepa], pay_server:lookup_dets(name, key, [apa, bepa, cepa])),

    ?assertEqual(1,meck:num_calls(db_w, lookup, ['_','_'])),
    true = meck:validate(db_w),
    ok = meck:unload(db_w).

verify_lookup_dets_find_test() ->
    ok = meck:new(db_w),
    ok = meck:expect(db_w, lookup, fun(_Name, _Key) -> [cepa, bepa] end),

    ?assertEqual([cepa, bepa], pay_server:lookup_dets(name, key, [apa, bepa, cepa])),

    ?assertEqual(1,meck:num_calls(db_w, lookup, ['_','_'])),
    true = meck:validate(db_w),
    ok = meck:unload(db_w).

verify_approved_debts_default_test() ->
    ok = meck:new(db_w),
    ok = meck:expect(db_w, lookup, fun(_Name, _Uid) -> debt_transactions() end),

    Res = pay_server:approved_debts(testuid1, name),

    ?assertEqual(approved_debts_fun(), Res),

    ?assertEqual(1,meck:num_calls(db_w, lookup, ['_','_'])),
    true = meck:validate(db_w),
    ok = meck:unload(db_w).

update_approved_debts_test() ->
    NewProps = new_approved_debts_fun(),

    ok = meck:new(db_w),
    ok = meck:expect(db_w, lookup, fun(_Name, _Key) -> debt_transactions() end),
    ok = meck:expect(db_w, delete, fun(_DName, _DKey) -> ok end),
    ok = meck:expect(db_w, insert, fun(_IName, IProp) -> ?assertEqual(IProp, updated_debt_transactions()) end),

    pay_server:update_approved_debts(testuid1, name, NewProps),

    ?assertEqual(1,meck:num_calls(db_w, delete, ['_','_'])),
    ?assertEqual(1,meck:num_calls(db_w, lookup, ['_','_'])),
    ?assertEqual(1,meck:num_calls(db_w, insert, ['_','_'])),
    true = meck:validate(db_w),
    ok = meck:unload(db_w).

verify_add_to_earlier_debt_test() ->
    ok = meck:new(db_w),
    ok = meck:expect(db_w, lookup, fun(_Name, Key) ->  [{Key, proplists:get_value(Key, debts_table())}] end),
    ok = meck:expect(db_w, insert, fun(_Name, {Key, Val1}) ->
                                           Val2 = proplists:get_value(Key, debts_table()),
                                           ?assertEqual(Val2 + 120, Val1)
                                   end),

    pay_server:add_to_earlier_debt(debt(), name),

    ?assertEqual(1,meck:num_calls(db_w, lookup, ['_','_'])),
    ?assertEqual(1,meck:num_calls(db_w, insert, ['_','_'])),
    true = meck:validate(db_w),
    ok = meck:unload(db_w).

verify_add_to_earlier_debt_empty_test() ->
    ok = meck:new(db_w),
    ok = meck:expect(db_w, lookup, fun(_Name, _Key) ->  [] end),
    ok = meck:expect(db_w, insert, fun(_Name, {_Key, Val1}) ->
                                           ?assertEqual(120, Val1)
                                   end),

    pay_server:add_to_earlier_debt(not_in_list_debt(), name),

    ?assertEqual(1,meck:num_calls(db_w, insert, ['_','_'])),
    ?assertEqual(1,meck:num_calls(db_w, lookup, ['_','_'])),
    true = meck:validate(db_w),
    ok = meck:unload(db_w).

verify_sort_user_debt_test() ->
    Debt = debt(),
    {Uuid, {P1, P2}, TimeStamp, Reason, Amount} = reversed_debt(Debt),
    Result = pay_server:sort_user_debt(Uuid, P1, P2, TimeStamp, Reason, Amount),
    ?assertEqual(Debt, Result).

verify_sort_user_debt_already_sorted_test() ->
    Debt = debt(),
    {Uuid, {P1, P2}, TimeStamp, Reason, Amount} = Debt,
    Result = pay_server:sort_user_debt(Uuid, P1, P2, TimeStamp, Reason, Amount),
    ?assertEqual(Debt, Result).


verify_call_get_users_test() ->
    UserDb = user_db(),
    ok = meck:new(db_w),
    ok = meck:expect(db_w, foldl, fun(Fun, Acc, users) -> lists:foldl(Fun, Acc, UserDb) end),
    Result = pay_server:handle_call(get_users, undefined, state()),

    Expected = {reply, lists:reverse(UserDb), state()},
    ?assertEqual(Expected, Result),

    ?assertEqual(1,meck:num_calls(db_w, foldl, '_')),
    true = meck:validate(db_w),
    ok = meck:unload(db_w).

verify_call_get_user_transactions_test() ->
    { [_User1|_] = _OtherUsers
    , _OtherDebts
    , [ApprovalDebt|_] = _OtherApprovalDebt
    , [V|_] = OtherDebt } = user_transactions(15),
    ok = meck:new(db_w),
    ok = meck:expect(db_w, lookup, fun(Any, Key) -> case Any of
                                                        ?DEBT_RECORD -> [proplists:lookup(Key, OtherDebt)];
                                                         _ -> [ApprovalDebt]
                                                     end
                                   end),
    Result = pay_server:handle_call({get_user_transactions, <<"user1">>}, undefined, state()),

    {U, {P1, P2}, T, R, A} = V,
    Expected = {reply, [{U, P1, P2, T, R, A}] , state()},
    ?assertEqual(Expected, Result),

    ?assertEqual(2,meck:num_calls(db_w, lookup, '_')),
    true = meck:validate(db_w),
    ok = meck:unload(db_w).

verify_call_get_debts_test() ->
    { [_User1|_] = _OtherUsers
    , OtherDebts
    , [_ApprovalDebt|_] = _OtherApprovalDebt
    , [_V|_] = _OtherDebt } = user_transactions(15),
    ok = meck:new(db_w),
    ok = meck:expect(db_w, foldl, fun(Fun, Acc, _Name) -> lists:foldl(Fun, Acc, OtherDebts)
                                   end),
    Result = pay_server:handle_call(get_debts, undefined, state()),
    Expected = {reply, lists:foldl(fun({{P1,P2}, Amount}, Acc) -> [{P1,P2,Amount}|Acc] end, [], OtherDebts), state()},
    ?assertEqual(Expected, Result),

    ?assertEqual(1,meck:num_calls(db_w, foldl, '_')),
    true = meck:validate(db_w),
    ok = meck:unload(db_w).


verify_call_get_transactions_test() ->
    { [_User1|_] = _OtherUsers
    , _OtherDebts
    , [_ApprovalDebt|_] = _OtherApprovalDebt
    , [_V|_] = OtherDebt } = user_transactions(15),
    ok = meck:new(db_w),
    ok = meck:expect(db_w, foldl, fun(Fun, Acc, _Name) -> lists:foldl(Fun, Acc, OtherDebt)
                                   end),
    Result = pay_server:handle_call(get_transactions, undefined, state()),
    Expected = {reply, lists:foldl(fun({Uuid, {Uuid1,Uuid2}, TimeStamp, Reason, Amount}, Acc) ->
                                  [{ Uuid
                                     , Uuid1
                                     , Uuid2
                                     , TimeStamp
                                     , Reason
                                     , Amount}|Acc] end, []
                          , OtherDebt), state()},
    ?assertEqual(Expected, Result),

    ?assertEqual(1,meck:num_calls(db_w, foldl, '_')),
    true = meck:validate(db_w),
    ok = meck:unload(db_w).

verify_call_get_usernames_test() ->
    { [_User1|_] = OtherUsers
    , _OtherDebts
    , [_ApprovalDebt|_] = _OtherApprovalDebt
    , [_V|_] = _OtherDebt } = user_transactions(15),
    Uids = lists:nthtail(10, lists:map(fun({Uid,_}) -> Uid end, OtherUsers)),
    ok = meck:new(db_w),
    ok = meck:expect(db_w, lookup, fun(_Name, Key) -> case Key of
                                                          <<"unknown">> -> [];
                                                          _ -> [proplists:lookup(Key, OtherUsers)] end end),

    Result = pay_server:handle_call({get_usernames, [<<"unknown">>|Uids]}, undefined, state()),
    Expected = { reply
               , [{error,user_not_found}|lists:map( fun({Uid, Username}) ->
                                   ?JSONSTRUCT([?UID(Uid), ?USER(Username)]) end
                          , lists:nthtail(10, OtherUsers))]
               , state()},
    ?assertEqual(Expected, Result),

    ?assertEqual(21,meck:num_calls(db_w, lookup, '_')),
    true = meck:validate(db_w),
    ok = meck:unload(db_w).

verify_call_add_normal_debt_test() ->
    { [_User1|_] = _OtherUsers
    , _OtherDebts
    , [_ApprovalDebt|_] = _OtherApprovalDebt
    , _OtherDebt } = user_transactions(15),
    Uid1 = <<"anders@gmail.com">>,
    Uid2 = <<"mattias@gmail.com">>,
    User1 = <<"Anders">>,
    User2 = <<"Mattias">>,
    Reason = <<"debt reason">>,
    Amount = 20,
    DebtToAdd = {?JSONSTRUCT, [ {?UID1, Uid1}
                              , {?UID2, Uid2}
                              , {?USER1, User1}
                              , {?USER2, User2}
                              , {?REASON, Reason}
                              , {?AMOUNT, Amount}
                              , {?TIMESTAMP, 30}
                              ]},

    %% Uids = lists:nthtail(10, lists:map(fun({Uid,_}) -> Uid end, OtherUsers)),
    ok = meck:new(db_w),
    ok = meck:expect(db_w, lookup
                     , fun(_Name, _Key) -> []
                       end),

    %%Check insert values
    ok = meck:expect( db_w, insert
                    , fun(Db, V) ->
                              case Db of
                                  ?USERS ->
                                      {Uid, Username} = V,
                                      ?assertEqual(true, lists:any(fun(Val) -> Uid =:= Val end, [Uid1, Uid2])),
                                      ?assertEqual(true, lists:any(fun(Val) -> Username =:= Val end, [User1, User2]));
                                  ?DEBTS ->
                                      {_Key, Amount2} = V,
                                      ?assertEqual(Amount, Amount2);
                                  _ -> ok
                              end

                      end),
    %% Check delete values
    ok = meck:expect(db_w, delete, fun(_,Val) -> ?assertEqual( true
                                                             , lists:any(fun(V) -> Val =:= V end,
                                                                       [Uid1, Uid2])
                                                            ),
                                                 ok end),

    {reply, Result, _} = pay_server:handle_call({add, DebtToAdd}, undefined, state()),
    Expected =[ {?UID1, Uid1}
              , {?USER1, User1}
              , {?UID2, Uid2}
              , {?USER2, User2}
              , {?REASON, Reason}
              , {?AMOUNT, 20}
              , {?TIMESTAMP, 30}
              , {?STATUS, <<"ok">>}],
    lists:map( fun({Type, Val1}) ->
                       case proplists:get_value(Type, Expected) of
                           undefined -> ?assertEqual(?UUID, Type);
                           Val2 -> ?assertEqual(Val1, Val2)
                       end
               end
             , Result),

    ?assertEqual(2,meck:num_calls(db_w, delete, '_')),
    ?assertEqual(6,meck:num_calls(db_w, insert, '_')),
    ?assertEqual(5,meck:num_calls(db_w, lookup, '_')),
    true = meck:validate(db_w),
    ok = meck:unload(db_w).

verify_call_add_debt_no_userids_test() ->
    { [_User1|_] = _OtherUsers
    , _OtherDebts
    , [_ApprovalDebt|_] = _OtherApprovalDebt
    , _OtherDebt } = user_transactions(15),
    User1 = <<"Anders">>,
    User2 = <<"Mattias">>,
    Reason = <<"debt reason">>,
    Amount = 20,
    DebtToAdd = {?JSONSTRUCT, [ {?USER1, User1}
                              , {?USER2, User2}
                              , {?REASON, Reason}
                              , {?AMOUNT, Amount}
                              , {?TIMESTAMP, 30}
                              ]},

    %% Uids = lists:nthtail(10, lists:map(fun({Uid,_}) -> Uid end, OtherUsers)),
    ok = meck:new(db_w),
    ok = meck:expect(db_w, lookup
                     , fun(_Name, _Key) -> []
                       end),

    %%Check insert values
    ok = meck:expect( db_w, insert
                    , fun(Db, V) ->
                              case Db of
                                  ?USERS ->
                                      {_Uid, Username} = V,
                                      ?assertEqual(true, lists:any(fun(Val) -> Username =:= Val end, [User1, User2]));
                                  ?DEBTS ->
                                      {_Key, Amount2} = V,
                                      ?assertEqual(true, lists:any(fun(Val) -> Val =:= Amount2 end, [Amount, -1*Amount]));
                                  _ -> ok
                              end

                      end),
    %% Check delete values
    ok = meck:expect(db_w, delete, fun(_,_Val) ->
                                                 ok end),

    {reply, Result, _} = pay_server:handle_call({add, DebtToAdd}, undefined, state()),
    Expected =[ {?USER1, User1}
              , {?USER2, User2}
              , {?REASON, Reason}
              , {?AMOUNT, Amount}
              , {?TIMESTAMP, 30}
              , {?STATUS, <<"ok">>}],
    lists:map( fun({Type, Val1}) ->
                       case proplists:get_value(Type, Expected) of
                           undefined -> ok ; %?assertEqual(?UUID, Type);
                           Val2 -> ?assertEqual(Val1, Val2)
                       end
               end
             , Result),

    ?assertEqual(5,meck:num_calls(db_w, lookup, '_')),
    ?assertEqual(6,meck:num_calls(db_w, insert, '_')),
    ?assertEqual(2,meck:num_calls(db_w, delete, '_')),
    true = meck:validate(db_w),
    ok = meck:unload(db_w).

verify_call_add_debt_existing_user_test() ->
    { [{Uid1, RealUser1}, {Uid2, RealUser2}| _] = OtherUsers
    , _OtherDebts
    , [_ApprovalDebt|_] = _OtherApprovalDebt
    , _OtherDebt } = user_transactions(15),
    User1 = <<"Anders">>,
    User2 = <<"Mattias">>,
    Reason = <<"debt reason">>,
    Amount = 20,
    DebtToAdd = {?JSONSTRUCT, [ {?UID1, Uid1}
                              , {?UID2, Uid2}
                              , {?USER1, User1}
                              , {?USER2, User2}
                              , {?REASON, Reason}
                              , {?AMOUNT, Amount}
                              , {?TIMESTAMP, 30}
                              ]},

    ok = meck:new(db_w),
    ok = meck:expect(db_w, lookup
                     , fun(Name, Key) ->
                               case Name of
                                   ?USERS -> [proplists:lookup(Key, OtherUsers)];
                                   _ -> []
                               end
                       end),

    %%Check insert values
    ok = meck:expect( db_w, insert
                    , fun(Db, V) ->
                              case Db of
                                  ?USERS ->
                                      {Uid, Username} = V,
                                      ?assertEqual(true, lists:any(fun(Val) -> Uid =:= Val end, [Uid1, Uid2])),
                                      ?assertEqual(true, lists:any(fun(Val) -> Username =:= Val end, [RealUser1, RealUser2]));
                                  ?DEBTS ->
                                      {_Key, Amount2} = V,
                                      ?assertEqual(true, lists:any(fun(Val) -> Val =:= Amount2 end, [Amount, -1*Amount]));
                                  _ -> ok
                              end

                      end),
    %% Check delete values
    ok = meck:expect(db_w, delete, fun(_,Val) -> ?assertEqual( true
                                                             , lists:any(fun(V) -> Val =:= V end,
                                                                       [Uid1, Uid2])
                                                            ),
                                                 ok end),

    {reply, Result, _} = pay_server:handle_call({add, DebtToAdd}, undefined, state()),
    Expected =[ {?UID1, Uid1}
              , {?USER1, RealUser1}
              , {?UID2, Uid2}
              , {?USER2, RealUser2}
              , {?REASON, Reason}
              , {?TIMESTAMP, 30}
              , {?STATUS, <<"ok">>}],
    lists:map( fun({Type, Val1}) ->
                       case proplists:get_value(Type, Expected) of
                           undefined -> ok ; %?assertEqual(?UUID, Type);
                           Val2 -> ?assertEqual(Val1, Val2)
                       end
               end
             , Result),

    ?assertEqual(5,meck:num_calls(db_w, lookup, '_')),
    ?assertEqual(4,meck:num_calls(db_w, insert, '_')),
    ?assertEqual(2,meck:num_calls(db_w, delete, '_')),

    true = meck:validate(db_w),
    ok = meck:unload(db_w).


verify_call_change_username_test() ->
    { [{User1, Username}|_] = _OtherUsers
    , _OtherDebts
    , [ApprovalDebt|_] = _OtherApprovalDebt
    , [_V|_] = OtherDebt } = user_transactions(15),
    NewUser = <<"anDeRsk84@gmail.com">>,
    NewUserModified = <<"andersk84@gmail.com">>,
    ok = meck:new(db_w),
    ok = meck:expect( db_w
                    , lookup
                    , fun(Name, Key) ->
                              case {Name, Key} of
                                  {?USERS, User1} -> [{User1, Username}];
                                  {?USERS, NewUserModified} -> [];
                                  {?USERS, _} -> ?assert(false);
                                  {?DEBT_APPROVAL_TRANSACTIONS, User1} -> [ApprovalDebt];
                                  {?DEBT_APPROVAL_TRANSACTIONS, User1} -> ?assert(false);
                                  {?DEBT_RECORD, Any} -> [proplists:lookup(Any, OtherDebt)];
                                  _      -> ?assert(false) end end),
    ok = meck:expect( db_w
                    , delete
                    , fun(Name, Key) ->
                              case {Name, Key} of
                                  {?DEBT_APPROVAL_TRANSACTIONS, User1} -> ?assert(true);
                                  {?USERS, User1} -> ?assert(true);
                                  {?DEBT_RECORD, _} -> ?assert(true);
                                  _ -> ?assert(false)
                              end
                              end),
    ok = meck:expect( db_w
                    , insert
                    , fun(Name, Key) ->
                              case {Name, Key} of
                                  {?USERS, {NewUserModified, Username}} -> ?assert(true);
                                  {?DEBT_RECORD, _} -> ?assert(true);
                                  _ -> ?assert(false)
                              end
                      end),

    pay_server:handle_cast({change_username, User1, NewUser}, state()),

    ?assertEqual(4,meck:num_calls(db_w, lookup, '_')),
    ?assertEqual(2,meck:num_calls(db_w, insert, '_')),
    ?assertEqual(3,meck:num_calls(db_w, delete, '_')),
    true = meck:validate(db_w),
    ok = meck:unload(db_w).


verify_call_transfer_debts_test() ->
    { [{User1, Username}|_] = _OtherUsers
    , _OtherDebts
    , [ApprovalDebt|_] = _OtherApprovalDebt
    , [_V|_] = OtherDebt } = user_transactions(15),
    NewUser = <<"andersk84@gmail.com">>,

    ok = meck:new(db_w),
    ok = meck:expect( db_w
                    , lookup
                    , fun(Name, Key) ->
                              case {Name, Key} of
                                  {?USERS, User1} -> [{User1, Username}];
                                  {?USERS, NewUser} -> [{NewUser,<<"">>}];
                                  {?USERS, _} -> ?assert(false);
                                  {?DEBT_APPROVAL_TRANSACTIONS, User1} -> [ApprovalDebt];
                                  {?DEBT_APPROVAL_TRANSACTIONS, User1} -> ?assert(false);
                                  {?DEBT_RECORD, Any} -> [proplists:lookup(Any, OtherDebt)];
                                  _      -> ?assert(false) end end),
    ok = meck:expect( db_w
                    , delete
                    , fun(Name, Key) ->
                              case {Name, Key} of
                                  {?DEBT_APPROVAL_TRANSACTIONS, User1} -> ?assert(true);
                                  {?USERS, User1} -> ?assert(true);
                                  {?DEBT_RECORD, _} -> ?assert(true);
                                  _ -> ?assert(false)
                              end
                              end),
    ok = meck:expect( db_w
                    , insert
                    , fun(Name, Key) ->
                              case {Name, Key} of
                                  {?USERS, {NewUser, Username}} -> ?assert(true);
                                  {?DEBT_RECORD, _} -> ?assert(true);
                                  _ -> ?assert(false)
                              end
                      end),

    pay_server:handle_cast({transfer_debts, User1, NewUser}, state()),

    ?assertEqual(3,meck:num_calls(db_w, lookup, '_')),
    ?assertEqual(1,meck:num_calls(db_w, insert, '_')),
    ?assertEqual(3,meck:num_calls(db_w, delete, '_')),
    true = meck:validate(db_w),
    ok = meck:unload(db_w).
