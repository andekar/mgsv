-module(debt).

-include("common.hrl").
-include("payapp.hrl").

-export([create_debttable/0, add/2, add_transaction/2, delete/1, delete/2, get/1,to_proplist/2]).

create_debttable() ->
    Res = mnesia:create_table( debt,
                               [ {type, set},
                                 {disc_copies, [node()]},
                                 {index, [uid1, uid1_username, uid2, uid2_username]},
                                 {attributes, record_info(fields, debt)}]),
    lager:info("Trying to create debt table with result ~p", [Res]).

add(#debt{} = D, ReqBy = #user{}) ->
    UD = D#debt{ edit_details = common:mod_edit_details(D#debt.edit_details, ReqBy)},
    {atomic,_} = mnesia:transaction(fun() -> mnesia:write(UD) end).

add_transaction(#transaction{} = T, ReqBy = #user{}) ->
    PaidB = T#transaction.paid_by,
    PaidF = T#transaction.paid_for,
    TCurrency = T#transaction.currency,
    case debt:get([{uid, PaidB},
                   {uid, PaidF}]) of
        [Debt] ->
            DCurrency = Debt#debt.currency,
            case {Debt#debt.uid1, Debt#debt.uid2, DCurrency} of
                {PaidB,PaidF, TCurrency} ->
                    debt:add(Debt#debt{amount = Debt#debt.amount +
                                           T#transaction.amount}, ReqBy),
                    T;
                {PaidF,PaidB, TCurrency} ->
                    debt:add(Debt#debt{amount = Debt#debt.amount -
                                           T#transaction.amount}, ReqBy),
                    T;
                {_P1,_P2, C} ->
                    OrgTrans = T#transaction.org_transaction,
                    OrgCurrency = OrgTrans#org_transaction.currency,
                    OrgAmount = OrgTrans#org_transaction.amount,
                    {res, Rate} = exchangerates_server:rate(OrgCurrency, C),
                    add_transaction(T#transaction{currency = C,
                                                  amount = OrgAmount * Rate},
                                   ReqBy)
            end;
        [] -> debt:add(#debt{
                          uid1 = T#transaction.paid_by,
                          uid1_username = T#transaction.paid_by_username,
                          uid2 = T#transaction.paid_for,
                          uid2_username = T#transaction.paid_for_username,
                          amount = T#transaction.amount,
                          currency = T#transaction.currency
                         }, ReqBy),
              T
    end.

delete(#debt{} = D) ->
    {atomic,Res} = mnesia:transaction(fun() ->
                                              mnesia:delete({debt,D#debt.id}) end),
    Res.

delete(#transaction{} = T, ReqBy = #user{}) ->
    debt:add_transaction(T#transaction{amount = T#transaction.amount * -1}, ReqBy).

get(User = #user{}) ->
    debt:get({uid, User#user.internal_uid});
get({uid, Uid}) ->
    {atomic,Res} = mnesia:transaction(fun() ->
                                              mnesia:index_read(debt, Uid, #debt.uid1) ++
                                                  mnesia:index_read(debt, Uid, #debt.uid2) end,[]),
    Res;
get({uid_username, Uid}) ->
    {atomic,Res} = mnesia:transaction(fun() ->
                                              mnesia:index_read(debt, Uid, #debt.uid1_username) ++
                                                  mnesia:index_read(debt, Uid, #debt.uid2_username) end),
    Res;
get([T1,T2]) ->
    Ts1 = debt:get(T1),
    Ts2 = debt:get(T2),
    Ts3 = Ts1 -- Ts2,
    Ts1 -- Ts3;
get(_) ->
    [].

to_proplist(Debt, Userdata) ->
    case Userdata#user_data.protocol of
        "0.36" ->
            to_proplist_36(Debt);
        _ ->
            to_proplist_old(Debt)
    end.

to_proplist_36(User) ->
    Fields = record_info(fields, debt),
    DFields = record_info(fields, edit_details),
    [_|Vals] = tuple_to_list(User),
    Ret = lists:zipwith(fun (edit_details, DY) ->
                                [_|DVals] = tuple_to_list(DY),
                                {<<"edit_details">>,
                                 ?JSONSTRUCT(lists:zipwith(fun(A,B) ->
                                                                   {atom_to_binary(A, utf8),B} end,
                                                           DFields, DVals))};
                            (X, Y) ->
                                {atom_to_binary(X, utf8),Y} end,
                        Fields,
                        Vals),
    lists:foldl(fun(ToRemove, State) ->
                        proplists:delete(ToRemove, State) end,
                Ret, [<<"uid1_username">>,
                      <<"uid2_username">>]
               ).

to_proplist_old(Debt) ->
    [{?UID1, Debt#debt.uid1_username},
     {?UID2, Debt#debt.uid2_username},
     {?AMOUNT, Debt#debt.amount},
    {?CURRENCY, Debt#debt.currency}].
