-module(pay_server).

-include("payapp.hrl").
-include("common.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-behaviour(gen_server).

-export([start_link/0]).

-export([ get_user_transactions/1
         , delete_debt/1
         , register_user/2
         , get_user_debt/1
         , change_user/2
         , add_debt/1
         , transfer_debts/3
         , user_exist/2
         , get_usernames/2
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

-export([currency_and_amount/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {_,E} = db_w:open_file("../../debt_feedback.dets",[{type, bag}]),
    {ok, [{?FEEDBACK, E}]}.

register_user(UserInfo, Userdata) ->
    gen_server:call(?MODULE, {register, UserInfo, Userdata}).

add_debt(Message) ->
    gen_server:call(?MODULE, Message).

delete_debt(Message) ->
    gen_server:call(?MODULE, Message).

get_user_debt(Userdata) ->
    gen_server:call(?MODULE, {get_user_debt, Userdata}).

change_userinfo(Ud, UserInfo) ->
    gen_server:call(?MODULE, {change_userinfo, Ud, UserInfo}).

get_user_transactions(Userdata) ->
    gen_server:call(?MODULE, {get_user_transactions, Userdata}).

get_usernames(Uids, Userdata) ->
    gen_server:call(?MODULE, {get_usernames, Uids, Userdata}).

change_user(OldUser, NewUser) ->
    gen_server:cast(?MODULE, {change_username, OldUser, NewUser}).

transfer_debts(OldUser, NewUser, ReqBy) ->
    gen_server:call(?MODULE, {transfer_debts, OldUser, NewUser, ReqBy}).

remove_user_debt(Uuid, Userdata) ->
    gen_server:cast(?MODULE,{remove_user_debt, Uuid, Userdata}).

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
    {reply, users(?GMAIL_USER), State};

handle_call(d_facebook_users, _From, State) ->
    {reply, users(?FACEBOOK_USER), State};

handle_call(d_local_users, _From, State) ->
    {reply, users(?LOCAL_USER), State};

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

%%TODO fix transaction
handle_call({change_userinfo, Ud, UserInfo}, _From, State) ->
    case Ud#user_data.user of
        User = #user{} ->
            UUser = users:update_parts(UserInfo, User),
            users:update(UUser, User),
            {reply, [users:to_proplist(UUser, Ud)], State};
        Other ->
            lager:error("Tried changing username got ~p from db Ud ~p Un ~p"
                       , [Other, Ud, UserInfo]),
            {reply, [], State}
        end;

handle_call({get_user_debt, U},  _From, State) ->
    User = U#user_data.user,
    Debts = debt:get(User),
    UDebts = lists:map(fun(V) ->
                               debt:to_proplist(V, U) end,
                               Debts),
    {reply, UDebts, State};

handle_call({get_user_transactions, Ud},  _From, State) ->
    User = Ud#user_data.user,
    Transactions = transaction:get(User),
    DebtLists =
        lists:map(fun(Transaction) ->
                          transaction:to_proplist(Transaction,Ud)
             end, Transactions),
    {reply, DebtLists, State};

%%TODO fix transactions
handle_call({get_usernames, Uids, Userdata}, _From, State) ->
    RecUsers = lists:filter(fun({?UID, _}) -> true;
                               (_) -> false end, Uids),
    RetUsers = lists:map(fun({?UID, TUid}) ->
                      Uid = ?UID_TO_LOWER(TUid),
                      case users:get(Uid) of
                          no_such_user -> [{error, user_not_found}];
                          User ->
                              ?JSONSTRUCT(users:to_proplist(User, Userdata))
                      end
              end, RecUsers),
    {reply, RetUsers, State};

handle_call({add, Ud, Struct}, _From, State) ->
    EchoUuid = proplists:lookup_all(?ECHO_UUID, Struct),
    ReqBy = Ud#user_data.user,
    Transaction = transaction:from_proplist(Struct),

    User1 = users:get({internal_uid, Transaction#transaction.paid_by}),
    User2 = users:get({internal_uid, Transaction#transaction.paid_for}),
    Username1 = User1#user.username,
    Username2 = User2#user.username,

    AddLocal = case { User1#user.user_type
                    , User2#user.user_type} of
                    %don't add debts between two localusers
                   {?LOCAL_USER, ?LOCAL_USER} ->
                       do_not_add_between_two_local_users;
                   _       -> ok
               end,
    {RT, Status} = case AddLocal of
                       ok ->
                           UT = debt:add_transaction(Transaction, ReqBy),
                           transaction:add(UT, ReqBy),
                           lager:info("Inserting transaction: ~p", [UT]),
                           Reason = UT#transaction.reason,
                           Message = list_to_binary(
                                       "A transaction was added with reason \""
                                       ++ binary_to_list(Reason)
                                       ++ "\" the transaction was added by "
                                       ++ binary_to_list(ReqBy#user.displayname)),
                           case ReqBy of %% TODO change this?
                               User1 -> pay_push_notification:notify_user(Username2, Message);
                               User2 -> pay_push_notification:notify_user(Username1, Message);
                               _ -> pay_push_notification:notify_user(Username1, Message),
                                    pay_push_notification:notify_user(Username2, Message)
                           end,
                           {UT, <<"ok">>};
                       _ -> {Transaction, <<"failed">>}
                   end,
    {reply, transaction:to_proplist(RT,Ud)  ++
         EchoUuid ++
         [?STATUS(Status)]
       , State};

%% should we send a notification to the other part?
handle_call({delete_debt, Userdata, Uuid}, _From, State) ->
    Transaction = transaction:get({transaction_id, Uuid}),
    ReqByU = Userdata#user_data.user,
    ReqBy = ReqByU#user.username,
    case Transaction of
        [#transaction{} = T] ->
            %crash if reqby is not one of the uids in the debt

            ok = case {T#transaction.paid_by_username,
                       T#transaction.paid_for_username} of
                     {ReqBy, _} -> ok;
                     {_ , ReqBy} -> ok;
                     _ -> {error, requestby_not_part_of_transaction}
                 end,
            transaction:delete(T),
            debt:delete(T, ReqByU),
            lager:info("DELETE DEBT uuid: ~p  Requested by: ~p ~n", [Uuid, ReqBy]);
        %% below shows something corrupt action should be taken
        [{_Uuid, _Items} | _More] = List -> lager:info("ERROR delete_debt ~p", [List]);
        _ -> ok
    end,
    {reply, ok, State};

handle_call({register, UserInfo, Ud}, _From, State) ->
    EchoUuid = proplists:lookup_all(?ECHO_UUID, UserInfo),
    UidLower  = verify_uid(uid_to_lower(uid(UserInfo)), []),
    RBy = case Ud#user_data.user of
              no_such_user -> UidLower;
              Us -> Us
          end,
    RepUserType = case string:rstr(binary_to_list(UidLower), "@") of
                      0 -> ?LOCAL_USER;
                      _ -> ?GMAIL_USER
                  end,
    UserType = proplists:get_value(?USER_TYPE, UserInfo, RepUserType), %%
    Currency = proplists:get_value(?CURRENCY, UserInfo, ?SWEDISH_CRONA),
    UserName = username(UserInfo),
    case UserName of
        UserName when is_binary(UserName) ->
            RetUsr = case users:get(UidLower) of
                         %% make sure that we never autogenerate username
                         no_such_user ->
                             User = users:add(UidLower,UidLower, UserName, UserType, Currency, RBy),
                             lager:info("Added user with uid ~p and username ~p UserType ~p currency ~p",
                                        [UidLower, UserName, UserType, Currency]),
                             User;
                         U  -> lager:info("User already exist"),
                               U
                     end,
            {reply, users:to_proplist(RetUsr, Ud) ++ EchoUuid,  State};
        _ -> {reply, [{<<"error">>, <<"unexpected_format">>}] ++ EchoUuid, State}
    end;

% The user transferring debts must be the creator (ReqBy)
handle_call({transfer_debts, TTOldUser, TTNewUser, Ud}, _From, State) ->
    try
        TOldUser = users:get(?UID_TO_LOWER(TTOldUser)),
        TNewUser = users:get(?UID_TO_LOWER(TTNewUser)),
        ReqByU = Ud#user_data.user,

        lager:info("TOldUser ~p Username ~p~nTNewUser ~p username ~p~nReqByU ~p username ~p", [TOldUser, ?UID_TO_LOWER(TTOldUser), TNewUser, ?UID_TO_LOWER(TTNewUser), ReqByU, ?UID_TO_LOWER(TTNewUser)]),
        %% Check that we do not transfer to the same user
        ok = case TOldUser#user.username == TNewUser#user.username of
                 true -> trying_to_transfer_to_self_error;
                 _ -> ok
             end,
        ok = case ReqByU#user.username == TNewUser#user.username of
                 true -> trying_to_add_to_self_error;
                 _ -> ok
             end,
                                                % to make sure we transfer only uuid users we check that the id does not contain an @
        0 = string:rstr(binary_to_list(TOldUser#user.username), "@"),

        [Debt] = debt:get([TOldUser,ReqByU]),
        [NewDebt] = debt:get([TNewUser,ReqByU]),

        %% currency must be same
        ok = case Debt#debt.currency == NewDebt#debt.currency of
                 true -> ok;
                 _ -> not_same_currency
             end,

        %% if there is just one debt
        %% BUT DO THIS LAST
        DeleteU =
            case {debt:get(TOldUser), TOldUser#user.user_type} of
                {[#debt{}], ?LOCAL_USER} ->
                    fun() -> users:delete(TOldUser) end;
                {[], ?LOCAL_USER} ->
                    fun() -> users:delete(TOldUser) end;
                _ -> fun() -> ok end
            end,    % delete the debt

        Ts = transaction:get(TOldUser),
        OldUsername = TOldUser#user.username,
        debt:delete(Debt),
        lists:foreach(
          fun(T) ->
                  UT = case T#transaction.paid_by_username of
                           OldUsername ->
                               T#transaction{paid_by_username = TNewUser#user.username,
                                             paid_by = TNewUser#user.internal_uid};
                           _ ->
                               T#transaction{paid_for_username = TNewUser#user.username,
                                             paid_for = TNewUser#user.internal_uid}
                       end,
                  RT = debt:add_transaction(UT, ReqByU),
                  transaction:add(RT, ReqByU)
          end, Ts),

        DeleteU(),
        lager:info("Transferred debts from ~p to ~p~n", [TOldUser, TNewUser]),
        {reply, ok, State}
    catch
        _:Error ->
            lager:alert("CRASH ~p~n~p", [Error, erlang:get_stacktrace()]),
            {reply, {nok, Error}, State}
    end;

handle_call(Request, _From, State) ->
    lager:alert("Handle unknown call ~p", [Request]),
    {reply, ok, State}.

%% notice how any one can change their username if we add this to calls
handle_cast({change_username, TTOldUser, TTNewUser}, State) ->
    TOldUser = ?UID_TO_LOWER(TTOldUser),
    TNewUser = ?UID_TO_LOWER(TTNewUser),

    TNewUser = verify_uid(TNewUser, []),

    no_such_user = users:get(TNewUser),
    User = #user{} = users:get(TOldUser),
    OUserid = User#user.internal_uid,
    ok = users:delete(User),
    UUser = User#user{
              uid = TNewUser,
              username = TNewUser
             },
    users:add(UUser, UUser), %% TODO when we have both uid and username

    lists:foreach(fun(T) ->
                          case T#transaction.paid_by_username of
                              TOldUser ->
                                  transaction:add(T#transaction{paid_by_username = TNewUser}, UUser);
                              _ ->
                                  transaction:add(T#transaction{paid_for_username = TNewUser}, UUser)
                          end end, transaction:get(TOldUser)),

    lists:foreach(fun(D) ->
                          case D#debt.uid1 of
                              OUserid ->
                                  debt:add(D#debt{uid1_username = TNewUser}, UUser);
                              _ ->
                                  debt:add(D#debt{uid2_username = TNewUser}, UUser)
                          end
                  end, debt:get({uid,OUserid})),

    lager:info("Changed username from ~p to ~p~n", [TOldUser, TNewUser]),
    {noreply, State};

handle_cast({remove_user_debt, TUuid, Userdata}, State) ->
    OUser = users:get(?UID_TO_LOWER(TUuid)),
    ReqUser = Userdata#user_data.user,
    lager:info("Removed debts between ~p and ~p ~n", [OUser, ReqUser]),

    %% if there is just one debt
    %% BUT DO THIS LAST
    DeleteU =
        case {debt:get(OUser), OUser#user.user_type} of
            {[#debt{}], ?LOCAL_USER} ->
                fun() -> users:delete(OUser) end;
            {[], ?LOCAL_USER} ->
                fun() -> users:delete(OUser) end;
        _ -> fun() -> ok end
    end,

    ok = case debt:get([OUser, ReqUser]) of
             [] ->
                 ok;
             [Debt] ->
                 lager:info("Removing debt ~p ~n", [Debt]),
                 debt:delete(Debt),
                 ok;
             _ -> nok
         end,
    Transactions = transaction:get([OUser, ReqUser]),

    lists:foreach(fun(T) ->
                          transaction:delete(T) end,
                 Transactions),
    DeleteU(),
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

terminate(Reason, _State) ->
    lager:emergency("TERMINATING ~p~n~p", [Reason, erlang:get_stacktrace()]),
    ok.

code_change(OldVsn, State, "0.3.6") ->
    lager:info("UPGRADING VERSION ~n~p~n~p~n~p~n",[OldVsn, State, "0.3.6"]),
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    lager:info("MNesia started, trying to create dbs",[]),
    users:create_mappingtable(),
    users:create_usertable(),
    lager:info("Trying to reconstruct users",[]),
    users:reconstruct(?USERS(State)),

    Users = ?USERS(State),
    db_w:close(Users),

    Debts = ?DEBTS(State),
    lager:info("trying to reconstruct debts"),
    debt:create_debttable(),
    debt:reconstruct(Debts),
    db_w:close(Debts),

    lager:info("trying to reconstruct transactions"),
    transaction:create_transactiontable(),
    TransactionsDb = ?DEBT_TRANSACTIONS(State),
    transaction:reconstruct(TransactionsDb),
    db_w:close(TransactionsDb),

    application:set_env(webmachine, server_name, "PayApp/0.3.6"),
    {ok, [{?FEEDBACK, ?FEEDBACK(State)}]};

code_change(OldVsn, State, Extra) ->
    lager:info("UPGRADING VERSION ~n~p~n~p~n~p~n",[OldVsn, State, Extra]),
    application:set_env(webmachine, server_name, "PayApp/" ++ Extra),
    {ok, State}.


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

% we might need to create a valid uid here,
% or if the uid already exist then we are fine
% or if the uid contains @
verify_uid(undefined, _Db) ->
    binary_uuid();
verify_uid(User, _Db) ->
    User.

uid_to_lower(undefined) ->
    lager:info("undefined uui"),
    undefined;
uid_to_lower(BinString) when is_binary(BinString) ->
    list_to_binary(string:to_lower(binary_to_list(BinString)));
uid_to_lower(Arg) ->
    lager:error("not valid String ~p", [Arg]).


user_exist(Uid, _State) ->
    UidLower  = ?UID_TO_LOWER(Uid),
    case users:get(UidLower) of
        no_such_user -> false;
        _  -> true
    end.

amount(Arg) ->
    props(?AMOUNT, Arg).

uid(Arg) ->
    props(?UID, Arg).

currency(Arg) ->
    props(?CURRENCY, Arg).

server_timestamp(Arg) ->
    props(?SERVER_TIMESTAMP, Arg).

username(Arg) ->
    props(?USER, Arg).

props(Name, Arg) when is_list(Arg) ->
    proplists:get_value(Name, Arg);
props(Name, {Name, Val}) ->
    Val;
props(Name, Arg) ->
    {Name, Arg}.


users(Type) ->
    users:count_by_usertype(Type).
