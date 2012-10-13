-module(pay_server).

-include("payapp.hrl").

-behaviour(gen_server).

-export([start_link/0, call_pay/1, cast_pay/1]).

-export([ sort_user_debt/6
         , add_to_earlier_debt/2
         , get_debts/0
         , get_transactions/0
         , get_user_transactions/1
         , get_user_debt/1
         , change_user/2
         , add_debt/1
         , transfer_debts/2
         , get_usernames/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, uuid_to_binary/1]).

-record(state, {?DEBTS, ?USERS, ?DEBT_RECORD}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {_,A} = dets:open_file("../../debts_0.2.dets",[{type, set}]),
    {_,B} = dets:open_file("../../users_0.2.dets",[{type, set}]),
    {_,C} = dets:open_file("../../debt_transactions_0.2b.dets",[{type, duplicate_bag}]),
    {_,D} = dets:open_file("../../debt_approval_transactions_0.2c.dets",[{type, set}]),
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

get_debts() ->
    io:format("Module: ~p function: get_debts~n", [?MODULE]),
    gen_server:call(?MODULE, get_debts).

get_user_debt(User) ->
    io:format("Module: ~p function: get_user_debt~n", [?MODULE]),
    gen_server:call(?MODULE, {get_user_debt, User}).

get_user_transactions(User) ->
    io:format("Module: ~p function: get_user_transactions~n", [?MODULE]),
    gen_server:call(?MODULE, {get_user_transactions, User}).

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
    UserList = dets:foldl(fun({Uuid, User}, Acc) -> [{Uuid, User}|Acc] end, [], Users),
    {reply, UserList, State};

handle_call({get_user_debt, User},  _From, State) ->
    Debts = ?DEBTS(State),
    DebtsList = dets:match(Debts, {{User, '$1'}, '$2'}),
    DebtsList2 = dets:match(Debts, {{'$1', User}, '$2'}),
    DebtLists = lists:map(fun([V1,V2]) -> {User,V1,V2} end, DebtsList),
    DebtLists2 = lists:map(fun([V1,V2]) -> {V1, User, V2} end, DebtsList2),
    {reply, DebtLists ++ DebtLists2, State};

handle_call({get_user_transactions, User},  _From, State) ->
    DebtRecord = ?DEBT_RECORD(State),
    error_logger:info_msg("getting user transactions for uuid: ~p~n", [User]),
    ApprovalDebt = ?DEBT_APPROVAL_TRANSACTIONS(State),
    error_logger:info_msg("user approval debts: ~p~n", [ApprovalDebt]),
    DebtIds = approved_debts(User, ApprovalDebt),
    error_logger:info_msg("user approval debt ids: ~p~n", [DebtIds]),
    DebtLists = lists:map(fun(Id) ->
                                  [{Uuid, {Uid1, Uid2}, Time, Reason, Amount}] = dets:lookup(DebtRecord, Id),
                                  {Uuid, Uid1, Uid2, Time, Reason, Amount}
                          end, DebtIds),
    {reply, DebtLists, State};

handle_call(get_debts, _From, State) ->
    Debts = proplists:get_value(?DEBTS, State),
    DebtList = dets:foldl(fun({{P1,P2}, Amount}, Acc) -> [{P1,P2,Amount}|Acc] end, [], Debts),
    {reply, DebtList, State};

handle_call(get_transactions, _From, State) ->
    Transactions = proplists:get_value(?DEBT_RECORD, State),
    DebtList = dets:foldl(fun({Uuid, {Uuid1,Uuid2}, TimeStamp, Reason, Amount}, Acc) ->
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
    RetUsers = lists:map(fun({_UID, Uid}) ->
                      case dets:lookup(Users, Uid) of
                          [] -> {error, user_not_found};
                          [{Uid, Username}] -> ?JSONSTRUCT([?UID(Uid), ?USER(Username)])
                      end
              end, Uids),
    {reply, RetUsers, State};

handle_call({add, {struct, Struct}}, _From, State) ->
    Debts = proplists:get_value(?DEBTS, State),
    DebtRecord = proplists:get_value(?DEBT_RECORD, State),
    Users = proplists:get_value(?USERS, State),
    Uid1 = proplists:get_value(?UID1, Struct, binary_uuid()),
    Uid2 = proplists:get_value(?UID2, Struct, binary_uuid()),
    P1 = proplists:get_value(?USER1, Struct, binary_uuid()),
    P2 = proplists:get_value(?USER2, Struct, binary_uuid()),
    Uuid = binary_uuid(),
    Reason = proplists:get_value(?REASON, Struct),
    Amount = proplists:get_value(?AMOUNT, Struct),
    TimeStamp = proplists:get_value(?TIMESTAMP, Struct, get_timestamp()),
    %insert people in to the database at first
    _Res = case dets:lookup(Users, Uid1) of
               [] -> dets:insert(Users, {Uid1, P1});
               _ -> nothing
           end,
    _Res2 = case dets:lookup(Users, Uid2) of
                [] -> dets:insert(Users, {Uid2, P2});
                _ -> nothing
            end,
    dets:insert(DebtRecord, sort_user_debt(Uuid, Uid1, Uid2, TimeStamp,  Reason, Amount)),
    add_to_earlier_debt(sort_user_debt(Uuid, Uid1, Uid2, TimeStamp, Reason, Amount), Debts),
    {reply, [ ?UID1(Uid1)
            , ?USER1(P1)
            , ?UID2(Uid2)
            , ?USER2(P2)
            , ?UUID(Uuid)
            , ?REASON(Reason)
            , ?AMOUNT(Amount)
            , ?TIMESTAMP(TimeStamp)
            , ?STATUS(<<"ok">>)
            ], State};

handle_call(_Request, _From, State) ->
    io:format("Module: ~p function: handle_call~nState~p~n", [?MODULE, State]),
    {reply, ok, State}.

%% notice how any one can change their username if we add this to calls
%% TODO change Debts database as well
handle_cast({change_username, TOldUser, TNewUser}, State) ->
    Debts = proplists:get_value(?DEBTS, State),
    Users = proplists:get_value(?USERS, State),
    DebtRecord = proplists:get_value(?DEBT_RECORD, State),
    %% we need to check if this is already an email account or not for case insensitivity
    StrOldUser = binary_to_list(TOldUser),
    StrNewUser = binary_to_list(TNewUser),
    OldUser = case string:rstr(StrOldUser, "@") of
                  0 -> TOldUser;
                  _ -> list_to_binary(string:to_lower(StrOldUser))
              end,
    NewUser = case string:rstr(StrNewUser, "@") of
                  0 -> no_user;
                  _ -> list_to_binary(string:to_lower(StrNewUser))
              end,
    [] = dets:lookup(Users,NewUser), %% make sure we do not create duplicate users.
    [{OldUser, User}] = dets:lookup(Users, OldUser),
    ok = dets:delete(Users, OldUser),
    dets:insert(Users,{NewUser, User}),

    DebtsList = dets:match(DebtRecord, {'$1', {OldUser, '$2'}, '$3', '$4', '$5'}),
    DebtsList2 = dets:match(DebtRecord, {'$1', {'$2', OldUser}, '$3', '$4', '$5'}),
    ok = dets:match_delete(DebtRecord, {'$1', {OldUser, '$2'}, '$3', '$4', '$5'}),
    ok = dets:match_delete(DebtRecord, {'$1', {'$2', OldUser}, '$3', '$4', '$5'}),
    lists:map(fun([Uuid, V1, V4, V2, V3]) -> dets:insert(DebtRecord, {Uuid, {NewUser, V1}, V4, V2, V3}), [] end, DebtsList),
    lists:map(fun([Uuid, V1, V4, V2, V3]) -> dets:insert(DebtRecord, {Uuid, {V1, NewUser}, V4, V2, V3}), [] end, DebtsList2),

    DList = dets:match(Debts, {{OldUser, '$1'}, '$2'}),
    DList2 = dets:match(Debts, {{'$1', OldUser}, '$2'}),
    ok = dets:match_delete(Debts, {{'$1', OldUser}, '$2'}),
    ok = dets:match_delete(Debts, {{OldUser, '$1'}, '$2'}),
    lists:map(fun([V1,V2]) -> dets:insert(Debts, {{NewUser,V1},V2}) end, DList),
    lists:map(fun([V1,V2]) -> dets:insert(Debts, {{V1, NewUser}, V2}) end, DList2),

    error_logger:info_msg("Changed username from ~p to ~p~n", [OldUser, NewUser]),
    {noreply, State};

% take care, any one might transfer debts to another person
% but he will have to guess the userid
handle_cast({transfer_debts, TOldUser, TNewUser}, State) ->
    Debts = proplists:get_value(?DEBTS, State),
    Users = proplists:get_value(?USERS, State),
    DebtRecord = proplists:get_value(?DEBT_RECORD, State),
    StrOldUser = binary_to_list(TOldUser),
    OldUser = case string:rstr(StrOldUser, "@") of
                  0 -> TOldUser;
                  _ -> list_to_binary(string:to_lower(StrOldUser))
              end,
    StrNewUser = binary_to_list(TNewUser),
    NewUser = list_to_binary(string:to_lower(StrNewUser)),
    0 = string:rstr(StrOldUser, "@"), % to make sure we transfer only uuid users we check that the id does not contain an @
    ok = dets:delete(Users, OldUser), % the new user must already exist
    NewUser = dets:lookup(Users, NewUser),%the user must exist already
    DebtsList = dets:match(DebtRecord, {'$1', {OldUser, '$2'}, '$3', '$4', '$5'}),
    DebtsList2 = dets:match(DebtRecord, {'$1', {'$2', OldUser}, '$3', '$4', '$5'}),
    ok = dets:match_delete(DebtRecord, {'$1', {OldUser, '$2'}, '$3', '$4', '$5'}),
    ok = dets:match_delete(DebtRecord, {'$1', {'$2', OldUser}, '$3', '$4', '$5'}),
    lists:map(fun([Uuid, V1, V4, V2, V3]) -> dets:insert(DebtRecord, {Uuid, {NewUser, V1}, V4, V2, V3}), [] end, DebtsList),
    lists:map(fun([Uuid, V1, V4, V2, V3]) -> dets:insert(DebtRecord, {Uuid, {V1, NewUser}, V4, V2, V3}), [] end, DebtsList2),

    DList = dets:match(Debts, {{OldUser, '$1'}, '$2'}),
    DList2 = dets:match(Debts, {{'$1', OldUser}, '$2'}),
    ok = dets:match_delete(Debts, {{'$1', OldUser}, '$2'}),
    ok = dets:match_delete(Debts, {{OldUser, '$1'}, '$2'}),
    lists:map(fun([V1,V2]) -> dets:insert(Debts, {NewUser,V1,V2}) end, DList),
    lists:map(fun([V1,V2]) -> dets:insert(Debts, {V1, NewUser, V2}) end, DList2),
    error_logger:info_msg("Transferred debts from ~p to ~p~n", [OldUser, NewUser]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Debts = proplists:get_value(?DEBTS, State),
    Users = proplists:get_value(?USERS, State),
    DebtRecord = proplists:get_value(?DEBT_RECORD, State),
    dets:close(Debts),
    dets:close(Users),
    dets:close(DebtRecord),
    ok.

code_change(OldVsn, State=#state{?DEBTS=Debts, ?USERS=Users, ?DEBT_RECORD=DebtRecord}, "0.1") ->
    error_logger:info_msg("UPGRADING VERSION ~n~p~n~p~n~p~n",[OldVsn, State, "0.1"]),
    {_, NewDebtRecord} = dets:open_file("../../debt_transactions_0.2.dets",[{type, duplicate_bag}]),
    {_, NewUsers} =  dets:open_file("../../users_0.2.dets",[{type, set}]),
    {_, NewDebts} =  dets:open_file("../../debts_0.2.dets",[{type, set}]),
    dets:traverse(Users, fun({User}) ->
                 Uuid = binary_uuid(),
                 error_logger:info_msg("INSERTING to table ~p~n",[{Uuid, User}]),
                 dets:insert(NewUsers, {Uuid, User}),
                 continue end),
    dets:traverse(DebtRecord, fun({{P1, P2}, Reason, Amount}) ->
                 [[Uuid1]] = dets:match(NewUsers, {'$1', P1}),
                 [[Uuid2]] = dets:match(NewUsers, {'$1', P2}),
                 TimeStamp = calendar:time_to_seconds(erlang:now()),
                 error_logger:info_msg("INSERTING to table ~p:~p ~p:~p ~p ~p~n~n~p",[P1, Uuid1, P2, Uuid2, TimeStamp, Reason, Amount]),
                 dets:insert(NewDebtRecord, {binary_uuid(), {Uuid1, Uuid2}, TimeStamp, Reason, Amount}),
                 continue end),
    dets:traverse(Debts, fun({{User1, User2}, Amount}) ->
                [[NewUser1]] = dets:match(NewUsers, {'$1', User1}),
                [[NewUser2]] = dets:match(NewUsers, {'$1', User2}),
                 error_logger:info_msg("INSERTING to table ~p:~p ~p:~p~n",[User1, NewUser1, User2, NewUser2]),
                dets:insert(NewDebts, {{NewUser1, NewUser2}, Amount}),
                continue end),
    dets:close(DebtRecord),
    dets:close(Users),
    dets:close(Debts),
    {ok, #state{?DEBTS=NewDebts, ?USERS=NewUsers, ?DEBT_RECORD=NewDebtRecord}};

code_change(OldVsn, State=#state{?DEBTS=Debts, ?USERS=Users, ?DEBT_RECORD=DebtRecord}, "0.2") ->
    error_logger:info_msg("UPGRADING VERSION ~n~p~n~p~n~p~n",[OldVsn, State, "0.2"]),
    {_, NewDebtRecord} = dets:open_file("../../debt_transactions_0.2b.dets",[{type, duplicate_bag}]),
    dets:traverse(DebtRecord, fun({Uuid, {P1, P2}, _T, Reason, Amount}) ->
                 TimeStamp = get_timestamp(),
                 error_logger:info_msg("INSERTING to table ~p ~p ~p ~p ~p ~p~n~n",[Uuid, P1, P2, TimeStamp, Reason, Amount]),
                 dets:insert(NewDebtRecord, {Uuid, {P1, P2}, TimeStamp, Reason, Amount}),
                 continue end),
    dets:close(DebtRecord),
    {ok, #state{?DEBTS=Debts, ?USERS=Users, ?DEBT_RECORD=NewDebtRecord}};

%% add approval of debts
%% add transferral of debts
%% add event groups
code_change(OldVsn, State=#state{?DEBTS=Debts, ?USERS=Users, ?DEBT_RECORD=DebtRecord}, "0.2b") ->
    error_logger:info_msg("UPGRADING VERSION ~n~p~n~p~n~p~n",[OldVsn, State, "0.2b"]),
    {_,NewApprovalTransactions} = dets:open_file("../../debt_approval_transactions_0.2c.dets",[{type, set}]),
    dets:traverse(DebtRecord, fun({Uuid, {P1, P2}, _TimeStamp, _Reason, _Amount}) ->
                 [{P1, DSet1}] = lookup_dets(NewApprovalTransactions, P1, [{P1, []}]),
                 [{P2, DSet2}] = lookup_dets(NewApprovalTransactions, P2, [{P2, []}]),
                 D1 = proplists:get_value(?APPROVED_DEBTS, DSet1, []),
                 D2 = proplists:get_value(?APPROVED_DEBTS, DSet2, []),
                 DSet11 = replace_prop(?APPROVED_DEBTS, DSet1, [Uuid | lists:delete(Uuid, D1)]),
                 DSet22 = replace_prop(?APPROVED_DEBTS, DSet2, [Uuid | lists:delete(Uuid, D2)]),
                 error_logger:info_msg("INSERTING to table approvaltransactions ~p for ~p : ~p and ~p : ~p",[Uuid, P1, DSet11, P2, DSet22]),
                 dets:insert(NewApprovalTransactions, {P1, DSet11}),
                 dets:insert(NewApprovalTransactions, {P2, DSet22}),
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
      case dets:lookup(Db, Key) of
           [] -> dets:insert(Db, {Key, Amount});
           [{_,Amount2}] -> dets:insert(Db, {Key, Amount + Amount2})
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
    case dets:lookup(Name, Key) of
        [] -> Default;
        Any -> Any
    end.

replace_prop(Key, Name, Value) ->
    [{Key, Value} | proplists:delete(Key, Name)].

approved_debts(Key, Name) ->
    [{_, Props}] = lookup_dets(Name, Key, [{any, []}]),
    error_logger:info_msg("approved debts: ~p~n", [Props]),
    ?APPROVED_DEBTS(Props).
