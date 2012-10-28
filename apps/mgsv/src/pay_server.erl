-module(pay_server).

-include("payapp.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-behaviour(gen_server).

-export([start_link/0, call_pay/1, cast_pay/1]).

-export([ sort_user_debt/6
         , add_to_earlier_debt/2
         , get_debts/0
         , get_transactions/0
         , get_user_transactions/1
         , user_not_approved_transactions/1
         , delete_debt/1
         , get_user_debt/1
         , change_user/2
         , add_debt/1
         , transfer_debts/2
         , approve_debt/1
         , get_usernames/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, uuid_to_binary/1]).

-record(state, {?DEBTS, ?USERS, ?DEBT_RECORD}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {_,A} = db_w:open_file("../../debts_0.2.dets",[{type, set}]),
    {_,B} = db_w:open_file("../../users_0.2.dets",[{type, set}]),
    {_,C} = db_w:open_file("../../debt_transactions_0.2b.dets",[{type, duplicate_bag}]),
    {_,D} = db_w:open_file("../../debt_approval_transactions_0.2c.dets",[{type, set}]),
    {ok, [{?DEBTS,A}, {?USERS, B}, {?DEBT_RECORD, C}, {?DEBT_APPROVAL_TRANSACTIONS, D}]}.

call_pay(Message) ->
    io:format("Module: ~p function: call_pay Argument: ~p~n", [?MODULE, Message]),
    gen_server:call(?MODULE, Message).

cast_pay(Message) ->
    io:format("Module: ~p function: cast_pay Argument: ~p~n", [?MODULE, Message]),
    gen_server:cast(?MODULE, Message).

add_debt(Message) ->
    error_logger:info_msg("Module: ~p function: add_debts Argument: ~p~n", [?MODULE, Message]),
    gen_server:call(?MODULE, Message).

approve_debt(Message) ->
    error_logger:info_msg("Module: ~p function: approve_debts Argument: ~p~n", [?MODULE, Message]),
    gen_server:call(?MODULE, Message).

delete_debt(Message) ->
    error_logger:info_msg("Module: ~p function: remove debt Argument: ~p~n", [?MODULE, Message]),
    gen_server:call(?MODULE, Message).

get_debts() ->
    io:format("Module: ~p function: get_debts~n", [?MODULE]),
    gen_server:call(?MODULE, get_debts).

get_user_debt(User) ->
    io:format("Module: ~p function: get_user_debt~n", [?MODULE]),
    gen_server:call(?MODULE, {get_user_debt, User}).

get_user_transactions(User) ->
    io:format("Module: ~p function: get_user_transactions~n", [?MODULE]),
    gen_server:call(?MODULE, {get_user_transactions, User}).

user_not_approved_transactions(User) ->
    io:format("Module: ~p function: user_not_approved_transactions~n", [?MODULE]),
    gen_server:call(?MODULE, {user_not_approved_transactions, User}).

get_transactions() ->
    io:format("Module: ~p function: get_transactions~n", [?MODULE]),
    gen_server:call(?MODULE, get_transactions).

get_usernames(Uids) ->
    error_logger:info_msg("Get usernames ~p",[Uids]),
    gen_server:call(?MODULE, {get_usernames, Uids}).

change_user(OldUser, NewUser) ->
    error_logger:info_msg("Changing username old: ~p new: ~p",[OldUser, NewUser]),
    gen_server:cast(?MODULE, {change_username, OldUser, NewUser}).

transfer_debts(OldUser, NewUser) ->
    error_logger:info_msg("Transferring debts from ~p to ~p~n", [OldUser, NewUser]),
    gen_server:cast(?MODULE, {transfer_debts, OldUser, NewUser}).

%% callbacks
handle_call(get_users, _From, State) ->
    Users = ?USERS(State),
    UserList = db_w:foldl(fun({Uuid, User}, Acc) -> [{Uuid, User}|Acc] end, [], Users),
    {reply, UserList, State};

handle_call({get_user_debt, User},  _From, State) ->
    {reply, get_tot_debts(?DEBTS(State), User), State};

handle_call({get_user_transactions, TUser},  _From, State) ->
    User = ?UID_TO_LOWER(TUser),
    DebtRecord = ?DEBT_RECORD(State),
    error_logger:info_msg("getting user transactions for uuid: ~p~n", [User]),
    ApprovalDebt = ?DEBT_APPROVAL_TRANSACTIONS(State),
    error_logger:info_msg("user approval debts: ~p~n", [ApprovalDebt]),
    DebtIds = approved_debts(User, ApprovalDebt),
    error_logger:info_msg("user approval debt ids: ~p~n", [DebtIds]),
    DebtLists = lists:map(fun(Id) ->
                                  [{Uuid, {Uid1, Uid2}, Time, Reason, Amount}] = db_w:lookup(DebtRecord, Id),
                                  {Uuid, Uid1, Uid2, Time, Reason, Amount}
                          end, DebtIds),
    {reply, DebtLists, State};

handle_call({user_not_approved_transactions, TUser},  _From, State) ->
    User = ?UID_TO_LOWER(TUser),
    DebtRecord = ?DEBT_RECORD(State),
    error_logger:info_msg("getting user transactions for uuid: ~p~n", [User]),
    ApprovalDebt = ?DEBT_APPROVAL_TRANSACTIONS(State),
    error_logger:info_msg("user approval debts: ~p~n", [ApprovalDebt]),
    DebtIds = not_approved_debts(User, ApprovalDebt),
    error_logger:info_msg("user not approval debt ids: ~p~n", [DebtIds]),
    DebtLists = lists:map(fun({Id,_}) ->
                                  [{Uuid, {Uid1, Uid2}, Time, Reason, Amount}] = db_w:lookup(DebtRecord, Id),
                                  {Uuid, Uid1, Uid2, Time, Reason, Amount}
                          end, DebtIds),
    {reply, DebtLists, State};

handle_call(get_debts, _From, State) ->
    Debts = ?DEBTS(State),
    DebtList = db_w:foldl(fun({{P1,P2}, Amount}, Acc) -> [{P1,P2,Amount}|Acc] end, [], Debts),
    {reply, DebtList, State};

handle_call(get_transactions, _From, State) ->
    Transactions = proplists:get_value(?DEBT_RECORD, State),
    DebtList = db_w:foldl(fun({Uuid, {Uuid1,Uuid2}, TimeStamp, Reason, Amount}, Acc) ->
                                  [{ Uuid
                                     , Uuid1
                                     , Uuid2
                                     , TimeStamp
                                     , Reason
                                     , Amount}|Acc] end, []
                          , Transactions),
    {reply, DebtList, State};

handle_call({get_usernames, Uids}, _From, State) ->
    Users = proplists:get_value(?USERS, State),
    RetUsers = lists:map(fun({?UID, TUid}) ->
                      Uid = ?UID_TO_LOWER(TUid),
                      case db_w:lookup(Users, Uid) of
                          [] -> {error, user_not_found};
                          [{Uid, Username}] ->
                              ?JSONSTRUCT([?UID(Uid), ?USER(Username)])
                      end
              end, Uids),
    {reply, RetUsers, State};

% so one of the uids should be equals to ReqBy
handle_call({add, TReqBy, {?JSONSTRUCT, Struct}}, _From, State) ->
    Debts = ?DEBTS(State),
    DebtRecord = ?DEBT_RECORD(State),
    Users = ?USERS(State),
    ApprovalDebt = ?DEBT_APPROVAL_TRANSACTIONS(State),
    Uuid = binary_uuid(),
    Reason = proplists:get_value(?REASON, Struct),
    Amount = proplists:get_value(?AMOUNT, Struct),
    TimeStamp = proplists:get_value(?TIMESTAMP, Struct, get_timestamp()),
    ReqBy = ?UID_TO_LOWER(TReqBy),
    %% per user
    [{P1ToUse, Uid1}, {P2ToUse, Uid2}]
        = lists:map(fun({P, U}) ->
                    Uid  = ?UID_TO_LOWER(verify_uid(proplists:get_value(U, Struct))),
                    PToUse = case db_w:lookup(Users, Uid) of
                                 %% make sure that we never autogenerate username
                                  [] -> [{P, User}] = proplists:lookup_all(P, Struct),
                                        db_w:insert(Users, {Uid, User}),
                                        User;
                                  [{_, R}]  -> R
                              end,
                    {PToUse, Uid} end,
                            [{?USER1, ?UID1}, {?USER2, ?UID2}]),
    %%update_approved_debts(Uid, ApprovalDebt, [Uuid]),
    %% this goes into unapproved debts if we are not one non existing user

    db_w:insert(DebtRecord, sort_user_debt(Uuid, Uid1, Uid2, TimeStamp, Reason, Amount)),
    %% this we only do when a debt has been approved!!!
    %%add_to_earlier_debt(sort_user_debt(Uuid, Uid1, Uid2, TimeStamp, Reason, Amount), Debts),
    _ = case {verify_uid(Uid1), verify_uid(Uid2)} of
            {Uid1,Uid2} -> add_not_approved_debt(Uid1, Uid2, ReqBy, ApprovalDebt, Uuid);
            _       -> add_approved_debt(Uid1, Uid2, ApprovalDebt, Uuid, sort_user_debt(Uuid, Uid1, Uid2, TimeStamp, Reason, Amount), Debts)
        end,
    {reply, [ ?UID1(Uid1)
            , ?USER1(P1ToUse)
            , ?UID2(Uid2)
            , ?USER2(P2ToUse)
            , ?UUID(Uuid)
            , ?REASON(Reason)
            , ?AMOUNT(Amount)
            , ?TIMESTAMP(TimeStamp)
            , ?STATUS(<<"ok">>)
            ], State};

handle_call({approve_debt, ReqBy, Uuid}, _From, State) ->
    ApprovalDebt = ?DEBT_APPROVAL_TRANSACTIONS(State),
    DebtRecord = ?DEBT_RECORD(State),
    Debts = ?DEBTS(State),
    %% get the not approved debt
    Props = get_not_approved_debt(ReqBy, ApprovalDebt, Uuid),
    ApprovedBy = ?APPROVED_BY(Props),
    %crash if we are not the one supposed to approve the debt
    ReqBy = ?NOT_APPROVED_BY(Props),
    error_logger:info_msg("Approving debt uuid: ~p  Requested by: ~p Approved by ~p Not approved by ~p~n", [Uuid, ReqBy, ApprovedBy, ReqBy]),
    % crash if there is no such debt
    [{Uuid, {Uid1, Uid2}, Time, Reason, Amount}] = db_w:lookup(DebtRecord, Uuid),
    %For both
    remove_not_approved_debt(ReqBy, ApprovalDebt, Uuid),
    remove_not_approved_debt(ApprovedBy, ApprovalDebt, Uuid),

    update_approved_debts(ReqBy, ApprovalDebt, [Uuid]),
    update_approved_debts(ApprovedBy, ApprovalDebt, [Uuid]),

    %get the debt so that we can update the total debts
    add_to_earlier_debt(sort_user_debt(Uuid, Uid1, Uid2, Time, Reason, Amount), Debts),
    {reply, ok, State};

handle_call({delete_debt, ReqBy, Uuid}, _From, State) ->
    ApprovalDebt = ?DEBT_APPROVAL_TRANSACTIONS(State),
    DebtRecord = ?DEBT_RECORD(State),
    %% get the not approved debt
    Props = get_not_approved_debt(ReqBy, ApprovalDebt, Uuid),
    ApprovedBy = ?APPROVED_BY(Props),
    %crash if we are not the one supposed to approve the debt
    %%ReqBy = ?NOT_APPROVED_BY(Props),
    ok = case {?NOT_APPROVED_BY(Props), ApprovedBy} of
             {ReqBy, _} -> ok;
             {_, ReqBy} -> ok;
             _          -> failed
         end,
    error_logger:info_msg("deleting not approved debt uuid: ~p  Requested by: ~p Approved by ~p Not approved by ~p~n", [Uuid, ReqBy, ApprovedBy, ReqBy]),
    db_w:delete(DebtRecord, Uuid),
    %For both
    remove_not_approved_debt(ReqBy, ApprovalDebt, Uuid),
    remove_not_approved_debt(ApprovedBy, ApprovalDebt, Uuid),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    io:format("Module: ~p function: handle_call~nState~p~n", [?MODULE, State]),
    {reply, ok, State}.

%% notice how any one can change their username if we add this to calls
%% TODO Add change of calculated debt
handle_cast({change_username, TTOldUser, TTNewUser}, State) ->
    Users = ?USERS(State),
    Debts = ?DEBTS(State),
    DebtRecord = ?DEBT_RECORD(State),
    ApprovalDebt = ?DEBT_APPROVAL_TRANSACTIONS(State),
    TOldUser = ?UID_TO_LOWER(TTOldUser),
    TNewUser = ?UID_TO_LOWER(TTNewUser),

    TNewUser = verify_uid(TNewUser),

    [] = db_w:lookup(Users, TNewUser), %% make sure we do not create duplicate users.
    [{TOldUser, Username}] = db_w:lookup(Users, TOldUser), %% make sure there exists an old user

    ok = db_w:delete(Users, TOldUser),
    db_w:insert(Users,{TNewUser, Username}),

    DebtIds = approved_debts(TOldUser, ApprovalDebt),

    ok = db_w:delete(ApprovalDebt, TOldUser),
    error_logger:info_msg("user approval debt ids: ~p~n", [DebtIds]),
    _DebtLists = lists:map(fun(Id) ->
                                  [{Uuid, {Uid1, Uid2}, Time, Reason, Amount}] = db_w:lookup(DebtRecord, Id),
                                  ok = db_w:delete(DebtRecord, Id),
                                  case Uid1 of
                                      TOldUser -> db_w:insert(DebtRecord, sort_user_debt(Uuid, TNewUser, Uid2, Time, Reason, Amount));
                                      _        -> db_w:insert(DebtRecord, sort_user_debt(Uuid, Uid1, TNewUser, Time, Reason, Amount))
                                  end
                          end, DebtIds),

    OldDebts = get_tot_debts(Debts, TOldUser),
    lists:foreach( fun({P1, P2, Amount}) ->
                           dets:delete(Debts, {P1,P2}),
                           case P1 of
                               TOldUser ->
                                   add_to_earlier_debt(sort_user_debt(undef, TNewUser, P2, undef, undef, Amount), Debts);
                               _ -> add_to_earlier_debt(sort_user_debt(undef, P1, TNewUser, undef, undef, Amount), Debts)
                           end
                   end
                 , OldDebts),

    error_logger:info_msg("Changed username from ~p to ~p~n", [TOldUser, TNewUser]),
    {noreply, State};

% take care, any one might transfer debts to another person
% but he will have to guess the userid
% note that these should be not approved debts
%% REMEMBER to change so that it changes debts as well
%% but that will not be needed when we add approval transactions
%% since they will appear as unapproved then. on the other hand we need to remove them from the db
handle_cast({transfer_debts, TTOldUser, TTNewUser}, State) ->
%    Debts = ?DEBTS(State),
    Users = ?USERS(State),
    DebtRecord = ?DEBT_RECORD(State),
    ApprovalDebt = ?DEBT_APPROVAL_TRANSACTIONS(State),

    TOldUser = ?UID_TO_LOWER(TTOldUser),
    TNewUser = ?UID_TO_LOWER(TTNewUser),
    % to make sure we transfer only uuid users we check that the id does not contain an @
    0 = string:rstr(binary_to_list(TOldUser), "@"),

    TNewUser = verify_uid(TNewUser),
    ok = db_w:delete(Users, TOldUser),

    [{TNewUser, _Username}] = db_w:lookup(Users, TNewUser),%the user must exist already

    DebtIds = approved_debts(TOldUser, ApprovalDebt),

    ok = db_w:delete(ApprovalDebt, TOldUser),
    error_logger:info_msg("user approval debt ids: ~p~n", [DebtIds]),
    _DebtLists = lists:map(fun(Id) ->
                                  [{Uuid, {Uid1, Uid2}, Time, Reason, Amount}] = db_w:lookup(DebtRecord, Id),
                                  ok = db_w:delete(DebtRecord, Id),
                                  case Uid1 of
                                      TOldUser -> db_w:insert(DebtRecord, sort_user_debt(Uuid, TNewUser, Uid2, Time, Reason, Amount));
                                      _        -> db_w:insert(DebtRecord, sort_user_debt(Uuid, Uid1, TNewUser, Time, Reason, Amount))
                                  end
                          end, DebtIds),

    error_logger:info_msg("Transferred debts from ~p to ~p~n", [TOldUser, TNewUser]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Debts = ?DEBTS(State),
    Users = ?USERS(State),
    DebtRecord = ?DEBT_RECORD(State),
    db_w:close(Debts),
    db_w:close(Users),
    db_w:close(DebtRecord),
    ok.

%% add approval of debts
%% add transferral of debts
%% add event groups
code_change(OldVsn, State=#state{?DEBTS=Debts, ?USERS=Users, ?DEBT_RECORD=DebtRecord}, "0.2b") ->
    error_logger:info_msg("UPGRADING VERSION ~n~p~n~p~n~p~n",[OldVsn, State, "0.2b"]),
    {_,NewApprovalTransactions} = db_w:open_file("../../debt_approval_transactions_0.2c.dets",[{type, set}]),
    db_w:traverse(DebtRecord, fun({Uuid, {P1, P2}, _TimeStamp, _Reason, _Amount}) ->
                 [{P1, DSet1}] = lookup_dets(NewApprovalTransactions, P1, [{P1, []}]),
                 [{P2, DSet2}] = lookup_dets(NewApprovalTransactions, P2, [{P2, []}]),
                 D1 = proplists:get_value(?APPROVED_DEBTS, DSet1, []),
                 D2 = proplists:get_value(?APPROVED_DEBTS, DSet2, []),
                 DSet11 = replace_prop(?APPROVED_DEBTS, DSet1, [Uuid | lists:delete(Uuid, D1)]),
                 DSet22 = replace_prop(?APPROVED_DEBTS, DSet2, [Uuid | lists:delete(Uuid, D2)]),
                 error_logger:info_msg("INSERTING to table approvaltransactions ~p for ~p : ~p and ~p : ~p",[Uuid, P1, DSet11, P2, DSet22]),
                 db_w:insert(NewApprovalTransactions, {P1, DSet11}),
                 db_w:insert(NewApprovalTransactions, {P2, DSet22}),
                 continue end),
    %[{approved, [DEBTS]}, {unapproved, [DEBTS]}]
    {ok, [{?DEBTS, Debts}, {?USERS, Users}, {?DEBT_RECORD, DebtRecord}, {?DEBT_APPROVAL_TRANSACTIONS, NewApprovalTransactions}]}; %%#state{?DEBTS=Debts, ?USERS=Users, ?DEBT_RECORD=NewDebtRecord}};

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


sort_user_debt(Uuid, P1, P2, TimeStamp, Reason, Amount) ->
     error_logger:info_msg("Adding debt: ~p, ~p, ~p, ~p, ~p, ~p ~n", [Uuid, P1, P2, TimeStamp, Reason, Amount]),
     case P1 < P2 of
          true -> {Uuid, {P1,P2}, TimeStamp, Reason, Amount};
          _ -> {Uuid, {P2,P1}, TimeStamp, Reason, (-1) * Amount}
     end.

%%Fix to be safe
add_to_earlier_debt({_Uuid, Key, _TimeStamp, _Reason, Amount}, Db) ->
      case db_w:lookup(Db, Key) of
           [] -> db_w:insert(Db, {Key, Amount});
           [{_,Amount2}] -> db_w:insert(Db, {Key, Amount + Amount2})
      end.

uuid_to_binary(Uuid) ->
     list_to_binary(uuid:to_string(Uuid)).

binary_uuid() ->
    ossp_uuid:make(v4, text).
%     list_to_binary(uuid:to_string(uuid:v4())).

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

approved_debts(Key, Name) ->
    [{_, Props}] = lookup_dets(Name, Key, [{any, []}]),
    error_logger:info_msg("approved debts: ~p~n", [Props]),
    ?APPROVED_DEBTS(Props).

not_approved_debts(Key, Name) ->
    [{_, Props}] = lookup_dets(Name, Key, [{any, []}]),
    error_logger:info_msg("not_approved debts: ~p~n", [Props]),
    ?NOT_APPROVED_DEBTS(Props).

%% key = uid
%% Name = table name
%% Items [items] list of items to insert
update_approved_debts(Key, Name, Items) ->
    [{_Key, Props}] = lookup_dets(Name, Key, [{any, []}]),
    ok = db_w:delete(Name, Key),
    error_logger:info_msg("updating approved debts: ~p~n", [?APPROVED_DEBTS(Props) ++ Items]),
    ok = db_w:insert(Name, {Key, replace_prop(?APPROVED_DEBTS, Props,
                                         ?APPROVED_DEBTS(Props) ++ Items)}).

update_not_approved_debts(Key, Name, Items) ->
    [{_Key, Props}] = lookup_dets(Name, Key, [{any, []}]),
    ok = db_w:delete(Name, Key),
    error_logger:info_msg("updating notapproved debts for user ~p : ~p~n", [Key, ?NOT_APPROVED_DEBTS(Props) ++ Items]),
    ok = db_w:insert(Name, {Key, replace_prop(?NOT_APPROVED_DEBTS, Props,
                                         ?NOT_APPROVED_DEBTS(Props) ++ Items)}).

remove_not_approved_debt(Key, Name, Item) ->
    [{_Key, Props}] = lookup_dets(Name, Key, [{any, []}]),
    ok = db_w:delete(Name, Key),
    NewEntry = proplists:delete(Item,?NOT_APPROVED_DEBTS(Props)),
    error_logger:info_msg("removing notapproved debts: ~p~n", [NewEntry]),
    ok = db_w:insert(Name, {Key, replace_prop(?NOT_APPROVED_DEBTS, Props,
                                         NewEntry)}).

get_not_approved_debt(Key, Name, Item) ->
    [{_Key, Props}] = lookup_dets(Name, Key, [{any, []}]),
    NotApproved = ?NOT_APPROVED_DEBTS(Props),
    proplists:get_value(Item, NotApproved).

verify_uid(undefined) ->
    binary_uuid();
verify_uid(User) ->
    ListUser = binary_to_list(User),
    case string:rstr(ListUser, "@") of
        0 -> invalid;
        _ -> User
    end.

get_tot_debts(Debts, User) ->
    DebtsList = db_w:match(Debts, {{User, '$1'}, '$2'}),
    DebtsList2 = db_w:match(Debts, {{'$1', User}, '$2'}),
    DebtLists = lists:map(fun([V1,V2]) -> {User,V1,V2} end, DebtsList),
    DebtLists2 = lists:map(fun([V1,V2]) -> {V1, User, V2} end, DebtsList2),
    DebtLists ++ DebtLists2.


add_approved_debt(Uid1, Uid2, ApprovalDebt, Uuid, SortedDebt, Debts) ->
    update_approved_debts(Uid1, ApprovalDebt, [Uuid]),
    update_approved_debts(Uid2, ApprovalDebt, [Uuid]),
    add_to_earlier_debt(SortedDebt, Debts).

add_not_approved_debt(Uid1, Uid2, ReqBy, ApprovalDebt, Uuid) ->
    case Uid1 of
        ReqBy -> ToAdd = [{Uuid, [{?APPROVED_BY, ReqBy}, {?NOT_APPROVED_BY, Uid2}]}],
                 update_not_approved_debts(Uid1, ApprovalDebt, ToAdd),
                 update_not_approved_debts(Uid2, ApprovalDebt, ToAdd);
        _     -> ToAdd = [{Uuid, [{?APPROVED_BY, ReqBy}, {?NOT_APPROVED_BY, Uid1}]}],
                 update_not_approved_debts(Uid1, ApprovalDebt, ToAdd),
                 update_not_approved_debts(Uid2, ApprovalDebt, ToAdd)
    end.
