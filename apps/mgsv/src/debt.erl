-module(debt).

-include("common.hrl").
-include("payapp.hrl").

-export([create_debttable/0, add/2, delete/1, delete/2, get/1,to_proplist/1,reconstruct/1]).

create_debttable() ->
    Res = mnesia:create_table( debt,
                               [ {type, set},
                                 {disc_copies, [node()]},
                                 {index, [uid1, uid1_username, uid2, uid2_username]},
                                 {attributes, record_info(fields, debt)}]),
    lager:info("Trying to create debt table with result ~p", [Res]).

add(#debt{} = D, ReqBy = #user{}) ->
    UD = D#debt{ edit_details = common:mod_edit_details(D#debt.edit_details, ReqBy)},
    mnesia:dirty_write(UD);
add(#transaction{} = T, ReqBy = #user{}) ->
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
                                           T#transaction.amount}, ReqBy);
                {PaidF,PaidB, TCurrency} ->
                    debt:add(Debt#debt{amount = Debt#debt.amount -
                                           T#transaction.amount}, ReqBy);
                _ -> error_not_same_currency
            end;
        [] -> debt:add(#debt{
                          uid1 = T#transaction.paid_by,
                          uid1_username = T#transaction.paid_by_username,
                          uid2 = T#transaction.paid_for,
                          uid2_username = T#transaction.paid_for_username,
                          amount = T#transaction.amount,
                          currency = T#transaction.currency
                         }, ReqBy)
    end.

delete(#debt{} = D) ->
    mnesia:dirty_delete(debt,D#debt.id).

delete(#transaction{} = T, ReqBy = #user{}) ->
    debt:add(T#transaction{amount = T#transaction.amount * -1}, ReqBy).

get(User = #user{}) ->
    debt:get({uid, User#user.internal_uid});
get({uid, Uid}) ->
        mnesia:dirty_index_read(debt, Uid, #debt.uid1) ++
        mnesia:dirty_index_read(debt, Uid, #debt.uid2);
get({uid_username, Uid}) ->
        mnesia:dirty_index_read(debt, Uid, #debt.uid1_username) ++
        mnesia:dirty_index_read(debt, Uid, #debt.uid2_username);
get([T1,T2]) ->
    Ts1 = debt:get(T1),
    Ts2 = debt:get(T2),
    Ts3 = Ts1 -- Ts2,
    Ts1 -- Ts3;
get(_) ->
    [].

to_proplist(Debt) ->
    [{?UID1, Debt#debt.uid1_username},
     {?UID2, Debt#debt.uid2_username},
     {?AMOUNT, Debt#debt.amount},
    {?CURRENCY, Debt#debt.currency}].

reconstruct(DebtsDB) ->
    dets:traverse(DebtsDB,
                  fun({{TUid1,TUid2}, Props}) ->
                          U1 = users:get(TUid1),
                          U2 = users:get(TUid2),
                          case {U1,U2} of
                              {no_such_user, _Any} ->
                                  lager:info("User1 ~p User2 ~p OldUID1 ~p OldUID2~p~n", [U1,U2, TUid1, TUid2]);
                              {_Any, no_such_user} ->
                                  lager:info("User1 ~p User2 ~p OldUID1 ~p OldUID2~p~n", [U1,U2, TUid1, TUid2]);
                              _ ->
                                  Uid1 = element(2,U1),
                                  Uid2 = element(2,U2),
                                  Currency = proplists:get_value(?CURRENCY, Props),
                                  Amount = proplists:get_value(?AMOUNT, Props),
                                  case Uid1 < Uid2 of
                                      true ->
                                          debt:add(#debt{
                                                      uid1 = Uid1,
                                                      uid1_username = TUid1,
                                                      uid2 = Uid2,
                                                      uid2_username = TUid2,
                                                      amount = Amount,
                                                      currency = Currency
                                                     }, U1);
                                      false ->
                                          debt:add(#debt{
                                                      uid1 = Uid2,
                                                      uid1_username = TUid2,
                                                      uid2 = Uid1,
                                                      uid2_username = TUid1,
                                                      amount = -1 * Amount,
                                                      currency = Currency
                                                     }, U1)
                                  end
                          end,
                          continue end).
