-module(mgsv_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/payapp.hrl").
-compile(export_all).

%% helpers

state() ->
    [{}].

not_in_list_debt() ->
    { pay_server:binary_uuid()
    , {testuid7, testuid8}
    , pay_server:get_timestamp()
    , <<"whatever reason">>
    , 120}.

debt() ->
    { pay_server:binary_uuid()
    , {testuid1, testuid2}
    , pay_server:get_timestamp()
    , <<"whatever reason">>
    , 120}.

reversed_debt(Debt) ->
    {Uuid, {P1, P2}, TimeStamp, Reason, Amount} = Debt,
    { Uuid
    , {P2, P1}
    , TimeStamp
    , Reason
    , -1* Amount}.

debt_transactions() ->
    {testuid1, user_debt_transactions()}.

user_debt_transactions() ->
    [{approved_debts, approved_debts_fun()}, {nonapproved_debts, nonapproved_debts_fun()}].

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

    true = meck:validate(db_w),
    ok = meck:unload(db_w).

verify_lookup_dets_find_test() ->
    ok = meck:new(db_w),
    ok = meck:expect(db_w, lookup, fun(_Name, _Key) -> [cepa, bepa] end),

    ?assertEqual([cepa, bepa], pay_server:lookup_dets(name, key, [apa, bepa, cepa])),

    true = meck:validate(db_w),
    ok = meck:unload(db_w).

verify_approved_debts_default_test() ->
    ok = meck:new(db_w),
    ok = meck:expect(db_w, lookup, fun(_Name, _Uid) -> [debt_transactions()] end),

    Res = pay_server:approved_debts(testuid1, name),

    ?assertEqual(approved_debts_fun(), Res),

    true = meck:validate(db_w),
    ok = meck:unload(db_w).

update_approved_debts_test() ->
    NewProps = new_approved_debts_fun(),

    ok = meck:new(db_w),
    ok = meck:expect(db_w, lookup, fun(_Name, _Key) -> [debt_transactions()] end),
    ok = meck:expect(db_w, delete, fun(_DName, _DKey) -> ok end),
    ok = meck:expect(db_w, insert, fun(_IName, IProp) -> ?assertEqual(IProp, updated_debt_transactions()) end),

    pay_server:update_approved_debts(testuid1, name, NewProps),

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

    true = meck:validate(db_w),
    ok = meck:unload(db_w).

verify_add_to_earlier_debt_empty_test() ->
    ok = meck:new(db_w),
    ok = meck:expect(db_w, lookup, fun(_Name, _Key) ->  [] end),
    ok = meck:expect(db_w, insert, fun(_Name, {_Key, Val1}) ->
                                           ?assertEqual(120, Val1)
                                   end),

    pay_server:add_to_earlier_debt(not_in_list_debt(), name),

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


%verify_call_get_users() ->

