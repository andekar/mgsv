-module(pay_server).

-include("payapp.hrl").
-include("common.hrl").

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
         , change_userinfo/2
         , remove_user_debt/2
         , add_feedback/2
         , get_feedback/0
         , remove_feedback/1
          %% debug functions
         , d_gmail_users/0
         , d_facebook_users/0
         , d_local_users/0
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, uuid_to_binary/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {_,A} = db_w:open_file("../../debts_0.3.2.dets",[{type, set}]),
    {_,B} = db_w:open_file("../../users_0.3.4.dets",[{type, set}]),
    {_,C} = db_w:open_file("../../debt_transactions_0.3.2.dets",[{type, set}]),
    {_,D} = db_w:open_file("../../debt_approval_transactions_0.2c.dets",[{type, set}]),
    {_,E} = db_w:open_file("../../debt_feedback.dets",[{type, bag}]),
    {ok, [ {?DEBTS,A}
         , {?USERS, B}
         , {?DEBT_TRANSACTIONS, C}
         , {?DEBT_APPROVAL_TRANSACTIONS, D}
         , {?FEEDBACK, E}]}.

call_pay(Message) ->
    gen_server:call(?MODULE, Message).

cast_pay(Message) ->
    gen_server:cast(?MODULE, Message).

register_user(UserInfo) ->
    gen_server:call(?MODULE, {register, UserInfo}).

user_exist(Uid) ->
    gen_server:call(?MODULE, {user_exist, Uid}).

add_debt(Message) ->
    gen_server:call(?MODULE, Message).

delete_debt(Message) ->
    gen_server:call(?MODULE, Message).

get_user_debt(User) ->
    gen_server:call(?MODULE, {get_user_debt, User}).

change_userinfo(Uid, UserInfo) ->
    gen_server:call(?MODULE, {change_userinfo, Uid, UserInfo}).

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

add_feedback(ReqBy, Feedback) ->
    gen_server:call(?MODULE, {add_feedback, ReqBy, Feedback}).

get_feedback() ->
    gen_server:call(?MODULE, {get_feedback}).

remove_feedback(Uuid) ->
    gen_server:cast(?MODULE, {remove_feedback, Uuid}).

%%DEBUG
d_gmail_users() ->
    gen_server:call(?MODULE, d_gmail_users).

d_facebook_users() ->
    gen_server:call(?MODULE, d_facebook_users).

d_local_users() ->
    gen_server:call(?MODULE, d_local_users).

%% callbacks
%%debug
handle_call(d_gmail_users, _From, State) ->
    Users = ?USERS(State),
    {reply, users(?GMAIL_USER, Users), State};

handle_call(d_facebook_users, _From, State) ->
    Users = ?USERS(State),
    {reply, users(?FACEBOOK_USER, Users), State};

handle_call(d_local_users, _From, State) ->
    Users = ?USERS(State),
    {reply, users(?LOCAL_USER, Users), State};

%% ordinary
handle_call(get_users, _From, State) ->
    Users = ?USERS(State),
    UserList = db_w:foldl(fun({_Uuid, PropList}, Acc) ->
                                  [PropList | Acc] end, [], Users),
    {reply, UserList, State};

handle_call({get_feedback}, _From, State) ->
    Feedback = ?FEEDBACK(State),
    FeedbackList = db_w:foldl(fun({_Uuid, PropList}, Acc) ->
                                      [PropList | Acc] end, [], Feedback),
    {reply, FeedbackList, State};

handle_call({add_feedback, ReqBy, Feedback}, _From, State) ->
    FeedDB = ?FEEDBACK(State),
    Uid = binary_uuid(),
    FB = [ {?UID, Uid}
         , ?REQUEST_BY(ReqBy)
         , {?FEEDBACK, Feedback}
         , server_timestamp(get_timestamp())
         ],
    db_w:insert(FeedDB, { Uid
                        , FB}),
    {reply, {ok, FB}, State};

handle_call({change_userinfo, Uid, UserInfo}, _From, State) ->
    Users = ?USERS(State),
    Res = case db_w:lookup(Users, Uid) of
              [{Uid, PropList}] ->
                  ToChange = [?USER, ?CURRENCY],
                  db_w:delete(Users, Uid),
                  NewProplist =
                      lists:foldl(fun(Change, Props) ->
                                          case proplists:get_value(Change, UserInfo) of
                                              undefined -> Props;
                                              Val -> replace_prop(Change, Props, Val)
                                          end
                                  end, PropList, ToChange),
                  db_w:insert(Users, {Uid, NewProplist}),
                  ok;
              Other ->
                  lager:error("Tried changing username got ~p from db Uid ~p Un ~p"
                              , [Other, Uid, UserInfo]),
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
    RecUsers = lists:filter(fun({?UID, _}) -> true;
                               (_) -> false end, Uids),
    RetUsers = lists:map(fun({?UID, TUid}) ->
                      Uid = ?UID_TO_LOWER(TUid),
                      case db_w:lookup(Users, Uid) of
                          [] -> [{error, user_not_found}];
                          [{Uid, PropList}] ->
                              ?JSONSTRUCT(PropList)
                      end
              end, RecUsers),
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
    ServerTimeStamp = server_timestamp(get_timestamp()),
    EchoUuid = proplists:lookup_all(?ECHO_UUID, Struct),
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
                                              , currency(Currency)
                                              , ServerTimeStamp],
                                       db_w:insert(Users, {Uid, List}),
                                       {Uid, List};
                                 [Val]  -> Val
                             end,
                            {Uid, PropList} end,
                    [{?USER1, ?UID1}, {?USER2, ?UID2}]),

    SortedUDebt = sort_user_debt(Uid1, Uid2, Amount),
    SortMult = Amount/amount(SortedUDebt),
    OrgDebt = case {amount(SortedUDebt) == Amount, proplists:lookup_all(?ORG_DEBT, Struct)} of
                  {_, []} -> [{?ORG_DEBT, [{?AMOUNT, amount(SortedUDebt)}, {?CURRENCY, Currency}]}];
                  {false, [{?ORG_DEBT, OrgD}]} -> [{?ORG_DEBT,  replace_prop(?AMOUNT, OrgD, -1* amount(OrgD))}];
                  {true, ODs} -> ODs
              end,
    {Curr, Am} = currency_and_amount({uid1(SortedUDebt), uid2(SortedUDebt)}
                                     , SortedUDebt ++ [ {?CURRENCY, Currency}]
                                     ++ OrgDebt, Debts),
    lager:info("got curr ~p Am ~p", [Curr, Am]),
    SortedDebt =
        sort_user_debt(uid1(SortedUDebt), uid2(SortedUDebt), Am)
        ++ get_and_check_props([?REASON], Struct) ++ [ timestamp(TimeStamp)
                                                    , {?CURRENCY, Curr}
                                                    , {?UUID, Uuid}
                                                    , ServerTimeStamp]
                                      ++ OrgDebt,
    %Check uid1 != uid2
    AddToSelf = case Uid1 of
                    Uid2 -> can_not_add_debt_to_self;
                    _ -> ok
                end,
    AddLocal = case { proplists:lookup(?USER_TYPE, PropList1)
                      , proplists:lookup(?USER_TYPE, PropList2)} of
                     %don't add debts between two localusers
                   {{?USER_TYPE, ?LOCAL_USER}, {?USER_TYPE, ?LOCAL_USER}} -> do_not_add_between_two_local_users;
                   _       -> ok
               end,
    Status = case {AddToSelf, AddLocal} of
                 {ok, ok} ->
                     add_transaction(SortedDebt, ApprovalDebt, Debts),
                     lager:info("Inserting Debt: ~p", [SortedDebt]),
                     db_w:insert(DebtTransactions, {Uuid, SortedDebt}),
                     case ReqBy of
                         Uid1 -> pay_push_notification:notify_user(Uid2, Reason);
                         Uid2 -> pay_push_notification:notify_user(Uid1, Reason);
                         _ -> pay_push_notification:notify_user(Uid1, Reason),
                              pay_push_notification:notify_user(Uid2, Reason)
                     end,
                     <<"ok">>;
                 _ -> <<"failed">>
             end,
    OD = proplists:get_value(?ORG_DEBT, OrgDebt),
    %% this should be changed to [{paid_by:[{user}]}, {paid_for:[user,user]}]
    {reply, [ % first user stuff
              ?UID1(Uid1)
            , ?USER1(username(PropList1))
            , ?USER_TYPE1(user_type(PropList1))

              % second user stuff
            , ?UID2(Uid2)
            , ?USER2(username(PropList2))
            , ?USER_TYPE2(user_type(PropList2))

            %% DEBT stuff
            , ?UUID(Uuid)
            , ?REASON(Reason)
            , ?AMOUNT(SortMult * amount(SortedDebt))     %% saved for backwardscompability
            , ?CURRENCY(Currency) %% saved for backwardscompability
            , timestamp(TimeStamp)
            , ServerTimeStamp
            , ?STATUS(Status)
            ] ++ EchoUuid
              ++ [{?ORG_DEBT,
                   replace_prop(?AMOUNT, OD, amount(OD) * SortMult)}]
       , State};

%% should we send a notification to the other part?
handle_call({delete_debt, ReqBy, Uuid}, _From, State) ->
    ApprovalDebt = ?DEBT_APPROVAL_TRANSACTIONS(State),
    DebtTransactions = ?DEBT_TRANSACTIONS(State),
    Debts = ?DEBTS(State),
    case db_w:lookup(DebtTransactions, Uuid) of
        [{Uuid, Items}] ->
            %crash if reqby is not one of the uids in the debt
            ok = case {uid1(Items), uid2(Items)} of
                     {ReqBy, _} -> ok;
                     {_ , ReqBy} -> ok;
                     _ -> {error, requestby_not_part_of_transaction}
                 end,
            db_w:delete(DebtTransactions, Uuid),
            Uid1 = proplists:get_value(?UID1, Items),
            Uid2 = proplists:get_value(?UID2, Items),
            Amount = proplists:get_value(?AMOUNT, Items), %% TODO fix this
            remove_debt(Uid1, ApprovalDebt, Uuid),
            remove_debt(Uid2, ApprovalDebt, Uuid),

            add_to_earlier_debt( -1* amount(sort_user_debt(Uid1, Uid2, Amount))
                                 , {Uid1, Uid2}
                                 , currency(Items)
                                 , Debts
                               ),
            lager:info("DELETE DEBT uuid: ~p  Requested by: ~p ~n", [Uuid, ReqBy]);
        %% below shows something corrupt action should be taken
        [{_Uuid, _Items} | _More] = List -> lager:info("ERROR delete_debt ~p", [List]);
        _ -> ok
    end,
    {reply, ok, State};

handle_call({register, UserInfo}, _From, State) ->
    Users = ?USERS(State),
    EchoUuid = proplists:lookup_all(?ECHO_UUID, UserInfo),
    UidLower  = verify_uid(uid_to_lower(uid(UserInfo)), Users),
    RepUserType = case string:rstr(binary_to_list(UidLower), "@") of
                      0 -> ?LOCAL_USER;
                      _ -> ?GMAIL_USER
                  end,
    UserType = proplists:get_value(?USER_TYPE, UserInfo, RepUserType), %%
    Currency = proplists:get_value(?CURRENCY, UserInfo, ?SWEDISH_CRONA),
    ServerTimestamp = server_timestamp(get_timestamp()),
    UserName = username(UserInfo),
    case UserName of
        UserName when is_binary(UserName) ->
            User = [ uid(UidLower)
                     , username(UserName)
                     , user_type(UserType)
                     , currency(Currency)
                     , ServerTimestamp] ++ EchoUuid,
            RetUsr = case db_w:lookup(Users, UidLower) of
                         %% make sure that we never autogenerate username
                         [] -> db_w:insert(Users, {UidLower, User}),
                               lager:info("Added user with uid ~p and username ~p UserType ~p currency ~p",
                                          [UidLower, username(UserName), UserType, Currency]),
                               User;
                         [{_UUid, U}]  -> lager:info("User already exist"),
                                          U
                     end,
            {reply, RetUsr,  State};
        _ -> {reply, [{<<"error">>, <<"unexpected_format">>}] ++ EchoUuid, State}
    end;

handle_call(Request, _From, State) ->
    lager:alert("Handle unknown call ~p", [Request]),
    {reply, ok, State}.

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

    Res = lists:filter(fun({{PP1, PP2}, _}) ->
                               case {PP1, PP2} of
                                   {ReqBy, _} -> true;
                                   {_, ReqBy} -> true;
                                   _ -> false
                               end end, ResDebts),
    case Res of
        [{{P1, P2}, _V}] ->
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

            lager:info("Removed debts from ~p ~n", [TOldUser]);
        [{{_,_},_}| _More] = List -> lager:info("ERROR in remove_user_debt ~p", [List]);
        _ -> ok
    end,

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
    ok = case ReqBy of
             TNewUser -> tryng_to_add_to_self_error;
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
    [{TNewUser, NPropList}] = db_w:lookup(Users, TNewUser),
    [{TOldUser, OPropList}] = db_w:lookup(Users, TOldUser),

    %% currency must be same
    OldCurr = currency(OPropList),
    ok = case currency(NPropList) of
             OldCurr -> ok;
             _ -> {error, currency_must_be_same}
         end,

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
                       add_to_earlier_debt(sort_user_debt(Uuid, uid1(SwappedItems), uid2(SwappedItems), undefined, undefined, amount(SwappedItems), undefined, currency(Items)), Debts),
                       update_approved_debts(TNewUser, ApprovalDebt, [Uuid]);
                   _ -> ok
               end
               end, DebtIds),

    lager:info("Transferred debts from ~p to ~p~n", [TOldUser, TNewUser]),
    {noreply, State};

handle_cast({remove_feedback, Uuid}, State) ->
    FeedDB = ?FEEDBACK(State),
    db_w:delete(FeedDB, Uuid),
    {noreply, State};

handle_cast(Msg, State) ->
    lager:alert("Handle unknown cast ~p", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    lager:alert("Handle unknown info ~p", [Msg]),
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

code_change(OldVsn, State, "0.3.6") ->
    lager:info("UPGRADING VERSION ~n~p~n~p~n~p~n",[OldVsn, State, "0.3.6"]),
    mnesia:start(),
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    users:reconstruct(?USERS(State)),
    application:set_env(webmachine, server_name, "PayApp/0.3.6"),
    {ok, State};

code_change(OldVsn, State, Extra) ->
    lager:info("UPGRADING VERSION ~n~p~n~p~n~p~n",[OldVsn, State, Extra]),
    application:set_env(webmachine, server_name, "PayApp/" ++ Extra),
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

currency_and_amount(Key, Props, DebtDb) ->
    case db_w:lookup(DebtDb, Key) of
        [] ->
%            lager:alert("nothing found in DB"),
            {currency(Props), amount(Props)};
        [{_,List}] ->
            Currency = currency(List),
%            lager:alert("looking at currency ~p", [Currency]),
            case {currency(Props), proplists:lookup_all(?ORG_DEBT, Props)} of
                {Currency, _} ->
%                    lager:alert("same currency"),
                    {currency(Props), amount(Props)};
                {Other, []} ->
%                    lager:alert("First Other looking up rate"),
                    {res, Rate} = exchangerates_server:rate(Other, Currency),
                    {Currency, Rate * amount(Props)};
                {_Other, [{?ORG_DEBT, OrgD}]} -> case currency(OrgD) of
                                                     Currency ->
%                                                         lager:alert("second Other using orgdebt"),
                                                         {Currency, amount(OrgD)};
                                                     OrgCurr ->
%                                                         lager:alert("third Other looking up rate"),
                                                         {res, Rate} = exchangerates_server:rate(OrgCurr, Currency),
                                                         {Currency, Rate * amount(OrgD)}
                                                 end
            end
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
%    lager:info("removing debts: ~p~n", [NewEntry]),
    ok = db_w:insert(Name, {Key, replace_prop(?APPROVED_DEBTS, Props,
                                         NewEntry)}).

% we might need to create a valid uid here,
% or if the uid already exist then we are fine
% or if the uid contains @
verify_uid(undefined, _Db) ->
    binary_uuid();
verify_uid(User, _Db) ->
    User.

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
    lager:info("undefined uui"),
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

timestamp(Arg) ->
    props(?TIMESTAMP, Arg).

server_timestamp(Arg) ->
    props(?SERVER_TIMESTAMP, Arg).

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


users(Type, Users) ->
    db_w:foldl(fun({_Uuid, PropList} = User, Acc) ->
                       case proplists:get_value(?USER_TYPE, PropList) of
                           Type -> [User | Acc];
                           _ -> Acc
                       end end, [], Users).
