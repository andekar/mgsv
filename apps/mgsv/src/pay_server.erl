-module(pay_server).

-include("payapp.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-behaviour(gen_server).

-export([start_link/0, call_pay/1, cast_pay/1]).

-export([ sort_user_debt/8
         , sort_user_debt/3
         , get_and_check_props/2
         , add_to_earlier_debt/2
         , add_to_earlier_debt/4
         , get_user_transactions/1
         , delete_debt/1
         , register_user/1
         , get_user_debt/1
         , change_user/2
         , add_debt/1
         , transfer_debts/3
         , user_exist/1
         , user_exist/2
         , get_usernames/1
         , change_username/2
         , remove_user_debt/2
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, uuid_to_binary/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {_,A} = db_w:open_file("../../debts_0.3.2.dets",[{type, set}]),
    {_,B} = db_w:open_file("../../users_0.3.2.dets",[{type, set}]),
    {_,C} = db_w:open_file("../../debt_transactions_0.3.2.dets",[{type, set}]),
    {_,D} = db_w:open_file("../../debt_approval_transactions_0.2c.dets",[{type, set}]),
    {ok, [ {?DEBTS,A}
         , {?USERS, B}
         , {?DEBT_TRANSACTIONS, C}
         , {?DEBT_APPROVAL_TRANSACTIONS, D}]}.

call_pay(Message) ->
    gen_server:call(?MODULE, Message).

cast_pay(Message) ->
    gen_server:cast(?MODULE, Message).

register_user(UserInfo) ->
    gen_server:cast(?MODULE, {register, UserInfo}).

user_exist(Uid) ->
    gen_server:call(?MODULE, {user_exist, Uid}).

add_debt(Message) ->
    gen_server:call(?MODULE, Message).

delete_debt(Message) ->
    gen_server:call(?MODULE, Message).

get_user_debt(User) ->
    gen_server:call(?MODULE, {get_user_debt, User}).

change_username(Uid, UserName) ->
    gen_server:call(?MODULE, {change_username, Uid, UserName}).

get_user_transactions(User) ->
    gen_server:call(?MODULE, {get_user_transactions, User}).

get_usernames(Uids) ->
    gen_server:call(?MODULE, {get_usernames, Uids}).

change_user(OldUser, NewUser) ->
    gen_server:cast(?MODULE, {change_username, OldUser, NewUser}).

transfer_debts(OldUser, NewUser, ReqBy) ->
    gen_server:cast(?MODULE, {transfer_debts, OldUser, NewUser, ReqBy}).

remove_user_debt(Uuid, ReqBy) ->
    gen_server:cast(?MODULE,{remove_user_debt, Uuid, ReqBy}).

%% callbacks
handle_call(get_users, _From, State) ->
    Users = ?USERS(State),
    UserList = db_w:foldl(fun({_Uuid, PropList}, Acc) ->
                                  [PropList | Acc] end, [], Users),
    {reply, UserList, State};

handle_call({change_username, Uid, UserName}, _From, State) ->
    Users = ?USERS(State),
    Res = case db_w:lookup(Users, Uid) of
            [{Uid, PropList}] ->
                db_w:delete(Users, Uid),
                db_w:insert(Users, {Uid, replace_prop(?USER, PropList, UserName)}),
                ok;
            Other ->
                lager:error("Tried changing username got ~p from db Uid ~p Un ~p"
                            , [Other, Uid, UserName]),
                error
        end,
    {reply, [Res], State};

handle_call({get_user_debt, User},  _From, State) ->
    {reply, get_tot_debts(?DEBTS(State), User), State};

handle_call({get_user_transactions, TUser},  _From, State) ->
    User = ?UID_TO_LOWER(TUser),
    DebtTransactions = ?DEBT_TRANSACTIONS(State),
    ApprovalDebt = ?DEBT_APPROVAL_TRANSACTIONS(State),
    DebtIds = approved_debts(User, ApprovalDebt),
    DebtLists =
        lists:map(fun(Id) ->
                  [{Id, List}] = db_w:lookup(DebtTransactions, Id),
                  List
             end, DebtIds),
    {reply, DebtLists, State};

handle_call({user_exists, Uid}, _From, State) ->
    {reply, user_exist(Uid, State), State};

handle_call({get_usernames, Uids}, _From, State) ->
    Users = proplists:get_value(?USERS, State),
    RetUsers = lists:map(fun({?UID, TUid}) ->
                      Uid = ?UID_TO_LOWER(TUid),
                      case db_w:lookup(Users, Uid) of
                          [] -> [{error, user_not_found}];
                          [{Uid, PropList}] ->
                              ?JSONSTRUCT(PropList)
                      end
              end, Uids),
    {reply, RetUsers, State};

handle_call({add, TReqBy, Struct}, _From, State) ->
    Debts = ?DEBTS(State),
    DebtTransactions = ?DEBT_TRANSACTIONS(State),
    Users = ?USERS(State),
    ApprovalDebt = ?DEBT_APPROVAL_TRANSACTIONS(State),

    Uuid = binary_uuid(),
    Reason = proplists:get_value(?REASON, Struct),
    Amount = proplists:get_value(?AMOUNT, Struct),
    Currency = proplists:get_value(?CURRENCY, Struct, ?SWEDISH_CRONA), %% Default to SEK for now
    TimeStamp = proplists:get_value(?TIMESTAMP, Struct, get_timestamp()),
    EchoUuid = proplists:lookup_all(?ECHO_UUID, Struct),
    Misc = ?MISC(proplists:get_value(?MISC, Struct, [])),
    ReqBy = ?UID_TO_LOWER(TReqBy),

    %% per user
     [{Uid1, PropList1}, {Uid2, PropList2}]
        = lists:map(fun({P, U}) -> %% TODO add validation of verify_uid
                    Uid  = verify_uid(uid_to_lower(proplists:get_value(U, Struct)), Users),
                    UserType = proplists:get_value(?USER_TYPE, Struct, ?LOCAL_USER),
                    {Uid, PropList} = case db_w:lookup(Users, Uid) of
                                      %% make sure that we never autogenerate username
                                 [] -> [{P, User}] = proplists:lookup_all(P, Struct),
                                       List = [ uid(Uid)
                                              , username(User)
                                              , user_type(UserType)
                                              , currency(Currency)],
                                       db_w:insert(Users, {Uid, List}),
                                       {Uid, List};
                                 [Val]  -> Val
                             end,
                            {Uid, PropList} end,
                    [{?USER1, ?UID1}, {?USER2, ?UID2}]),
    SortedDebt = sort_user_debt(Uid1, Uid2, Amount)
                 ++ get_and_check_props([?REASON], Struct) ++ [ {?TIMESTAMP, TimeStamp}
                                                    , {?CURRENCY, Currency}
                                                    , {?UUID, Uuid}],
    ok = case { proplists:lookup(?USER_TYPE, PropList1)
              , proplists:lookup(?USER_TYPE, PropList2)} of
             %don't add debts between two localusers
             {{?USER_TYPE, ?LOCAL_USER}, {?USER_TYPE, ?LOCAL_USER}} -> fail;
             _       ->
                 add_transaction(SortedDebt, ApprovalDebt, Debts)
        end,

    lager:debug("Inserting Debt: ~p", SortedDebt),
    db_w:insert(DebtTransactions, {Uuid, SortedDebt}),
    _ = case ReqBy of
            Uid1 -> pay_push_notification:notify_user(Uid2, Reason);
            Uid2 -> pay_push_notification:notify_user(Uid1, Reason);
            _ -> pay_push_notification:notify_user(Uid1, Reason),
                 pay_push_notification:notify_user(Uid2, Reason)
        end,
    {reply, [ ?UID1(Uid1)
            , ?USER1(username(PropList1))
            , ?UID2(Uid2)
            , ?USER2(username(PropList2))
            , ?UUID(Uuid)
            , ?REASON(Reason)
            , ?AMOUNT(Amount)
            , ?TIMESTAMP(TimeStamp)
            , ?STATUS(<<"ok">>)
            , Misc] ++ EchoUuid, State};

%% should we send a notification to the other part?
handle_call({delete_debt, ReqBy, Uuid}, _From, State) ->
    ApprovalDebt = ?DEBT_APPROVAL_TRANSACTIONS(State),
    DebtTransactions = ?DEBT_TRANSACTIONS(State),
    Debts = ?DEBTS(State),
    % crash if there is no such debt
    [{Uuid, Items}] = db_w:lookup(DebtTransactions, Uuid),

    db_w:delete(DebtTransactions, Uuid),
    Uid1 = proplists:get_value(?UID1, Items),
    Uid2 = proplists:get_value(?UID2, Items),
    Amount = proplists:get_value(?AMOUNT, Items),
    remove_debt(Uid1, ApprovalDebt, Uuid),
    remove_debt(Uid2, ApprovalDebt, Uuid),

    add_to_earlier_debt( -1* amount(sort_user_debt(Uid1, Uid2, Amount))
                       , {Uid1, Uid2}
                       , currency(Items)
                       , Debts
                       ),
    lager:info("deleting debt uuid: ~p  Requested by: ~p ~n", [Uuid, ReqBy]),
    {reply, ok, State};

handle_call(Request, _From, State) ->
    lager:alert("Handle unknown call ~p", Request),
    {reply, ok, State}.

handle_cast({register, UserInfo}, State) ->
    Users = ?USERS(State),
    UidLower  = verify_uid(?UID_TO_LOWER(uid(UserInfo)), Users),
    RepUserType = case string:rstr(binary_to_list(UidLower), "@") of
                      0 -> ?LOCAL_USER;
                      _ -> ?GMAIL_USER
                  end,
    UserType = proplists:get_value(?USER_TYPE, UserInfo, RepUserType), %%
    Currency = proplists:get_value(?CURRENCY, UserInfo, ?SWEDISH_CRONA),
    case db_w:lookup(Users, UidLower) of
        %% make sure that we never autogenerate username
        [] -> db_w:insert(Users, {UidLower, [ uid(UidLower)
                                            , username(username(UserInfo))
                                            , user_type(UserType)
                                            , currency(Currency)]}),
              lager:info("Added user with uid ~p and username ~p UserType ~p currency ~p", [UidLower, ?USER(UserInfo), UserType, Currency]);
        _  -> lager:info("User already exist")
    end,
    {noreply, State};

%% notice how any one can change their username if we add this to calls
handle_cast({change_username, TTOldUser, TTNewUser}, State) ->
    Users = ?USERS(State),
    Debts = ?DEBTS(State),
    DebtTransactions = ?DEBT_TRANSACTIONS(State),
    ApprovalDebt = ?DEBT_APPROVAL_TRANSACTIONS(State),
    TOldUser = ?UID_TO_LOWER(TTOldUser),
    TNewUser = ?UID_TO_LOWER(TTNewUser),

    TNewUser = verify_uid(TNewUser, Users),

    [] = db_w:lookup(Users, TNewUser), %% make sure we do not create duplicate users.
    [{TOldUser, PropList}] = db_w:lookup(Users, TOldUser), %% make sure there exists an old user

    ok = db_w:delete(Users, TOldUser),
    db_w:insert(Users, {TNewUser, replace_prop(?UID, PropList, TNewUser)}),

    DebtIds = approved_debts(TOldUser, ApprovalDebt),

    ok = db_w:delete(ApprovalDebt, TOldUser),
    update_approved_debts(TNewUser, ApprovalDebt, DebtIds),

    _DebtLists =
        lists:map(fun(Id) ->
                          [{Uuid, Items}] = db_w:lookup(DebtTransactions, Id),
                          ok = db_w:delete(DebtTransactions, Id),
                          SwappedItems = change_uid(TNewUser, TOldUser, Items),
                          db_w:insert(DebtTransactions, {Uuid, SwappedItems})
                  end, DebtIds),

    OldDebts = get_tot_debts(Debts, TOldUser),
    lists:foreach( fun({{P1, P2}, List}) ->
                           dets:delete(Debts, {P1,P2}),
                           case P1 of
                               TOldUser ->
                                   add_to_earlier_debt(sort_user_debt(undef, TNewUser, P2, undef, undef, amount(List), undef, currency(List)), Debts);
                               _ -> add_to_earlier_debt(sort_user_debt(undef, P1, TNewUser, undef, undef, amount(List), undef, currency(List)), Debts)
                           end
                   end
                 , OldDebts),

    lager:info("Changed username from ~p to ~p~n", [TOldUser, TNewUser]),
    {noreply, State};

handle_cast({remove_user_debt, TUuid, TReqBy}, State) ->
    Debts = ?DEBTS(State),
    Users = ?USERS(State),
    DebtTransactions = ?DEBT_TRANSACTIONS(State),
    ApprovalDebt = ?DEBT_APPROVAL_TRANSACTIONS(State),

    [{TOldUser, PropList}] = db_w:lookup(Users, ?UID_TO_LOWER(TUuid)),
    OldUserType = proplists:get_value(?USER_TYPE, PropList),
    ReqBy = ?UID_TO_LOWER(TReqBy),
    ResDebts = get_tot_debts(Debts, TOldUser),

    [{{P1, P2}, _V}] = lists:filter(fun({{PP1, PP2}, _}) ->
                                          case {PP1, PP2} of
                                              {ReqBy, _} -> true;
                                              {_, ReqBy} -> true;
                                              _ -> false
                                          end end, ResDebts),

    % delete the debt
    db_w:delete(Debts, {P1, P2}),

    % all debts are approved
    DebtIds = approved_debts(TOldUser, ApprovalDebt),

    %% if there is just one debt and we have a local user here
    case {ResDebts, OldUserType} of
        {{P1, P2, _}, ?LOCAL_USER} ->
            ok = db_w:delete(Users, TOldUser),
            ok = db_w:delete(ApprovalDebt, TOldUser); %% should not cause any problems
        _ -> ok
    end,

    _DebtLists = lists:map(
                   fun(Id) ->
                           [{Id, Items}] = db_w:lookup(DebtTransactions, Id),
                           case {get_value(?UID1, Items), get_value(?UID2, Items)} of
                               {P1, P2} ->         %delete the approved debt id
                                   ok = remove_debt(TOldUser, ApprovalDebt, Id),
                                   ok = remove_debt(ReqBy, ApprovalDebt, Id);
                               _ -> ok
                           end
                   end, DebtIds),

    lager:info("Removed debts from ~p ~n", [TOldUser]),
    {noreply, State};

% The user transferring debts must be the creator (ReqBy)
handle_cast({transfer_debts, TTOldUser, TTNewUser, ReqBy}, State) ->
    Debts = ?DEBTS(State),
    Users = ?USERS(State),
    DebtTransactions = ?DEBT_TRANSACTIONS(State),
    ApprovalDebt = ?DEBT_APPROVAL_TRANSACTIONS(State),

    TOldUser = ?UID_TO_LOWER(TTOldUser),
    TNewUser = ?UID_TO_LOWER(TTNewUser),
    %% Check that we do not transfer to the same user
    ok = case TOldUser of
             TNewUser -> trying_to_transfer_to_self_error;
             _ -> ok
         end,
    % to make sure we transfer only uuid users we check that the id does not contain an @
    0 = string:rstr(binary_to_list(TOldUser), "@"),

    % we should only get one old debt since these are supposed to be per user.
    % but we allow there to be more
    ResDebts = get_tot_debts(Debts, TOldUser),

    [{{P1, P2}, _L}] = lists:filter(fun({{PP1, PP2}, _}) ->
                                            case {PP1, PP2} of
                                                {ReqBy, _} -> true;
                                                {_, ReqBy} -> true;
                                                _ -> false
                                            end end, ResDebts),

    %the user must exist already
    [{TNewUser, _PropList}] = db_w:lookup(Users, TNewUser),

    % delete the debt
    db_w:delete(Debts, {P1, P2}),

    TNewUser = verify_uid(TNewUser, Users),

    DebtIds = approved_debts(TOldUser, ApprovalDebt),

    %% if there is just one debt
    case ResDebts of
        [{{P1, P2}, _}] ->
            ok = db_w:delete(Users, TOldUser),
            ok = db_w:delete(ApprovalDebt, TOldUser); %% should not cause any problems
        _ -> ok
    end,

    lists:map(
      fun(Id) ->
              [{Uuid, Items}] = db_w:lookup(DebtTransactions, Id),
               case {get_value(?UID1, Items), get_value(?UID2, Items)} of
                   {P1, P2} ->
                       ok = db_w:delete(DebtTransactions, Id),
                       SwappedItems = change_uid(TNewUser, TOldUser, Items),
                       db_w:insert(DebtTransactions, {Uuid, SwappedItems}),
                       add_to_earlier_debt(sort_user_debt(Uuid, uid1(SwappedItems), uid2(SwappedItems), undefined, undefined, amount(Items), undefined, currency(Items)), Debts),
                       update_approved_debts(TNewUser, ApprovalDebt, [Uuid]);
                   _ -> ok
               end
               end, DebtIds),

    lager:info("Transferred debts from ~p to ~p~n", [TOldUser, TNewUser]),
    {noreply, State};

handle_cast(Msg, State) ->
    lager:alert("Handle unknown cast ~p", Msg),
    {noreply, State}.

handle_info(Msg, State) ->
    lager:alert("Handle unknown info ~p", Msg),
    {noreply, State}.

terminate(Reason, State) ->
    lager:emergency("TERMINATING ~p", [Reason]),
    Debts = ?DEBTS(State),
    Users = ?USERS(State),
    DebtTransactions = ?DEBT_TRANSACTIONS(State),
    db_w:close(Debts),
    db_w:close(Users),
    db_w:close(DebtTransactions),
    ok.

code_change(OldVsn, State, "0.3.2") ->
    lager:info("UPGRADING VERSION ~n~p~n~p~n~p~n",[OldVsn, State, "0.3.2"]),
    Users = ?USERS(State),
    DebtRecord = ?DEBT_RECORD(State),
    Debts = ?DEBTS(State),
    {_,A} = db_w:open_file("../../debts_0.3.2.dets",[{type, set}]),
    {_,B} = db_w:open_file("../../users_0.3.2.dets",[{type, set}]),
    {_,C} = db_w:open_file("../../debt_transactions_0.3.2.dets",[{type, set}]),
    db_w:traverse(Users,
                  fun({Uuid, Username}) ->
                      _R = case string:rstr(binary_to_list(Uuid), "@") of
                          0 -> db_w:insert(B, {Uuid, [ uid(Uuid)
                                                     , username(Username)
                                                     , user_type(?LOCAL_USER)
                                                     , currency(?SWEDISH_CRONA)]});
                          _ -> db_w:insert(B, {Uuid, [ uid(Uuid)
                                                     , username(Username)
                                                     , user_type(?GMAIL_USER)
                                                     , currency(?SWEDISH_CRONA)]})
                      end,
                  continue
                  end),
    db_w:traverse(DebtRecord,
                  fun({Uuid, {Uid1, Uid2}, Time, Reason, Amount}) ->
                          db_w:insert(C, { Uuid
                                         , [ {?UUID, Uuid}
                                         , {?UID1, Uid1}
                                         , {?UID2, Uid2}
                                         , {?TIMESTAMP, Time}
                                         , {?SERVER_TIMESTAMP, get_timestamp()}
                                         , {?REASON, Reason}
                                         , {?AMOUNT, Amount}
                                         , {?CURRENCY, ?SWEDISH_CRONA}]}),
                  continue
                  end),
    db_w:traverse(Debts,
                  fun({{P1, P2}, Amount}) ->
                          db_w:insert(A, {{P1, P2}, [ {?AMOUNT, Amount}
                                                    , {?CURRENCY, ?SWEDISH_CRONA}
                                                    , {?UID1, P1}
                                                    , {?UID2, P2}]}), %%DEFAULT to SEK
                  continue
                  end),
    db_w:close(Users),
    db_w:close(DebtRecord),
    db_w:close(Debts),
    {ok, [ {?DEBTS,A}
         , {?USERS, B}
         , {?DEBT_TRANSACTIONS, C}
         , {?DEBT_APPROVAL_TRANSACTIONS, ?DEBT_APPROVAL_TRANSACTIONS(State)}]};

code_change(OldVsn, State, Extra) ->
    lager:info("UPGRADING VERSION ~n~p~n~p~n~p~n",[OldVsn, State, Extra]),
    {ok, State}.


sort_user_debt(P1, P2, Amount) ->
    case P1 < P2 of
          true -> [{?UID1, P1}, {?UID2, P2}, {?AMOUNT, Amount}];
                  %%{Uuid, {P1,P2}, TimeStamp, Reason, Amount, Misc, Currency};
          _ -> [{?UID1, P2}, {?UID2, P1}, {?AMOUNT, (-1) * Amount}]
               %%{Uuid, {P2,P1}, TimeStamp, Reason, (-1) * Amount, Misc, Currency}
     end.

sort_user_debt(Uuid, P1, P2, TimeStamp, Reason, Amount, Misc, Currency) ->
     case P1 < P2 of
          true -> {Uuid, {P1,P2}, TimeStamp, Reason, Amount, Misc, Currency};
          _ -> {Uuid, {P2,P1}, TimeStamp, Reason, (-1) * Amount, Misc, Currency}
     end.

%%Fix to be safe
add_to_earlier_debt({_Uuid, Key = {Uid1, Uid2}, _TimeStamp, _Reason, Amount, _Misc, Currency}, Debts) ->
    lager:info("Adding to earlier debt ~p Amount ~p", [Key, Amount]),
    case db_w:lookup(Debts, Key) of
        [] -> db_w:insert(Debts, {Key, [ amount(Amount)
                                       , currency(Currency)
                                       , uid1(Uid1)
                                       , uid2(Uid2)]});
        [{_,List}] ->
            Currency = currency(List),
            db_w:insert(Debts, {Key, update_prop(?AMOUNT, List, fun(OldAmount) -> Amount + OldAmount end)});
        _ -> error
    end.

add_to_earlier_debt(Amount, Key = {Uid1, Uid2}, Currency, Debts) ->
    lager:info("Adding to earlier debt ~p Amount ~p Currency ~p", [Key, Amount, Currency]),
    case db_w:lookup(Debts, Key) of
        [] -> db_w:insert(Debts,  {Key, [ amount(Amount)
                                        , currency(Currency)
                                        , uid1(Uid1)
                                        , uid2(Uid2)]});
        [{Key, List}] ->
            Currency = currency(List),
            db_w:insert(Debts,{Key, update_prop(?AMOUNT, List, fun(OldAmount) -> Amount + OldAmount end)});
        _ -> error
    end.



uuid_to_binary(Uuid) ->
     list_to_binary(uuid:to_string(Uuid)).

binary_uuid() ->
    ossp_uuid:make(v4, text).

get_timestamp() ->
    {Mega, Seconds, Milli} = erlang:now(),
    Mega * 1000000000000 + Seconds * 1000000 + Milli.

lookup_dets(Name, Key, Default) ->
    case db_w:lookup(Name, Key) of
        []  -> Default;
        Any -> Any
    end.

replace_prop(Key, List, Value) ->
    [{Key, Value} | proplists:delete(Key, List)].

update_prop(Key, List, Fun) ->
    FirstVal = proplists:get_value(Key, List), %% note unsafe
    [{Key, Fun(FirstVal)} | proplists:delete(Key, List)].

approved_debts(Key, Name) ->
    [{_, Props}] = lookup_dets(Name, Key, [{any, []}]),
    ?APPROVED_DEBTS(Props).

%% key = uid
%% Name = table name
%% Items [items] list of items to insert
update_approved_debts(Key, Name, Items) ->
    [{_Key, Props}] = lookup_dets(Name, Key, [{any, []}]),
    ok = db_w:delete(Name, Key),
    lager:info("updating approved debts for ~p with ~p", [Key, Items]),
    ok = db_w:insert(Name, {Key, replace_prop(?APPROVED_DEBTS, Props,
                                         ?APPROVED_DEBTS(Props) ++ Items)}).

remove_debt(Key, Name, Item) ->
    [{_Key, Props}] = lookup_dets(Name, Key, [{any, []}]),
    ok = db_w:delete(Name, Key),
    NewEntry = lists:delete(Item,?APPROVED_DEBTS(Props)),
    lager:info("removing debts: ~p~n", [NewEntry]),
    ok = db_w:insert(Name, {Key, replace_prop(?APPROVED_DEBTS, Props,
                                         NewEntry)}).

% we might need to create a valid uid here,
% or if the uid already exist then we are fine
% or if the uid contains @
verify_uid(undefined, _Db) ->
    binary_uuid();
verify_uid(User, Db) ->
    case contains_at(User) of
        invalid -> case db_w:lookup(Db, User) of
                 [] -> invalid;
                 _ -> User
             end;
        _ -> User
    end.

contains_at(User) ->
    ListUser = binary_to_list(User),
    case string:rstr(ListUser, "@") of
        0 -> invalid;
        _ -> User
    end.

get_tot_debts(Debts, User) ->
    DebtsList  = db_w:match(Debts, {{User, '$1'}, '$2'}),
    DebtsList2 = db_w:match(Debts, {{'$1', User}, '$2'}),
    DebtLists  = lists:map(fun([V1,V2]) -> {{User, V1}, V2} end, DebtsList),
    DebtLists2 = lists:map(fun([V1,V2]) -> {{V1, User}, V2} end, DebtsList2),
    DebtLists ++ DebtLists2.


%% add_approved_debt(Uid1, Uid2, ApprovalDebt, Uuid, SortedDebt, Debts) ->
%%     ok = add_to_earlier_debt(SortedDebt, Debts),
%%     update_approved_debts(Uid1, ApprovalDebt, [Uuid]),
%%     update_approved_debts(Uid2, ApprovalDebt, [Uuid]).

add_transaction(Props, ApprovalDebts, Debts) ->
    Uuid = proplists:get_value(?UUID, Props),
    add_to_earlier_debt( proplists:get_value(?AMOUNT, Props)
                       , { proplists:get_value(?UID1, Props)
                         , proplists:get_value(?UID2, Props)}
                      , proplists:get_value(?CURRENCY, Props)
                      , Debts),
    update_approved_debts(proplists:get_value(?UID1, Props), ApprovalDebts, [Uuid]),
    update_approved_debts(proplists:get_value(?UID2, Props), ApprovalDebts, [Uuid]).

uid_to_lower(undefined) ->
    undefined;
uid_to_lower(BinString) when is_binary(BinString) ->
    list_to_binary(string:to_lower(binary_to_list(BinString)));
uid_to_lower(Arg) ->
    lager:error("not valid String ~p", [Arg]).


user_exist(Uid, State) ->
    Users = ?USERS(State),
    UidLower  = ?UID_TO_LOWER(Uid),
    case db_w:lookup(Users, UidLower) of
        [] -> false;
        _  -> true
    end.

get_and_check_props(Props, PropList) ->
    lists:map(fun(Prop) ->
                      [{Prop, _} = Property] = proplists:lookup_all(Prop, PropList),
                      Property
              end, Props).

get_value(Key, Items) ->
    proplists:get_value(Key, Items).


%% Change one UID in a debt
%% this swaps UID1 and UID2 if needed
change_uid(NewUid, OldUid, Items) ->
    ClearedItems = lists:foldl(fun(Key, Acc) ->
                                       proplists:delete(Key, Acc) end,
                               Items, [?UID1, ?UID2, ?AMOUNT]),
    case get_value(?UID1, Items) of
        OldUid ->
            sort_user_debt(NewUid, get_value(?UID2, Items), get_value(?AMOUNT, Items))
                ++ ClearedItems;
        _        ->
            sort_user_debt(get_value(?UID1, Items), NewUid, get_value(?AMOUNT, Items))
                ++ ClearedItems
    end.


amount(Arg) ->
    props(?AMOUNT, Arg).

uid(Arg) ->
    props(?UID, Arg).

uid1(Arg) ->
    props(?UID1, Arg).

uid2(Arg) ->
    props(?UID2, Arg).

currency(Arg) ->
    props(?CURRENCY, Arg).

user_type(Arg) ->
    props(?USER_TYPE, Arg).

username(Arg) ->
    props(?USER, Arg).

props(Name, Arg) when is_list(Arg) ->
    proplists:get_value(Name, Arg);
props(Name, {Name, Val}) ->
    Val;
props(Name, Arg) ->
    {Name, Arg}.
