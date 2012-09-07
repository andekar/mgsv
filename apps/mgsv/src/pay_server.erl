-module(pay_server).

-behaviour(gen_server).

-export([start_link/0, call_pay/1, cast_pay/1]).

-export([sort_user_debt/4, add_to_earlier_debt/2, get_debts/0, get_transactions/0, get_user_transactions/1, get_user_debt/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {debts, users, debt_record}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {_,A} = dets:open_file("../../debts.dets",[{type, set}]),
    {_,B} = dets:open_file("../../users.dets",[{type, set}]),
    {_,C} = dets:open_file("../../debt_transactions.dets",[{type, duplicate_bag}]),
    {ok, #state{debts=A, users=B, debt_record=C}}.

call_pay(Message) ->
    io:format("Module: ~p function: call_pay Argument: ~p~n", [?MODULE, Message]),
    gen_server:call(?MODULE, Message).

cast_pay(Message) ->
    io:format("Module: ~p function: cast_pay Argument: ~p~n", [?MODULE, Message]),
    gen_server:cast(?MODULE, Message).

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

%% callbacks
handle_call(get_users, _From, State=#state{debts=_Debts, users=Users, debt_record=_DebtRecord}) ->
    UserList = dets:foldl(fun(Arg, Acc) -> [Arg|Acc] end, [],Users),
    {reply, UserList, State};

handle_call({get_user_debt, User},  _From, State=#state{debts=Debts, users=_Users, debt_record=_DebtRecord}) ->
    DebtsList = dets:match(Debts, {{User, '$1'}, '$2'}),
    DebtsList2 = dets:match(Debts, {{'$1', User}, '$2'}),
    DebtList = lists:map(fun([V1,V2]) -> {User,V1,V2} end, DebtsList),
    DebtList2 = lists:map(fun([V1,V2]) -> {V1, User, V2} end, DebtsList2),
    {reply, DebtList ++ DebtList2, State};

handle_call({get_user_transactions, User},  _From, State=#state{debts=_Debts, users=_Users, debt_record=DebtRecord}) ->
    DebtsList = dets:match(DebtRecord, {{User, '$1'}, '$3', '$2'}),
    DebtsList2 = dets:match(DebtRecord, {{'$1', User}, '$3', '$2'}),
    DebtList = lists:map(fun([V1,V2, V3]) -> {User, V1, V2, V3} end, DebtsList),
    DebtList2 = lists:map(fun([V1,V2, V3]) -> {V1, User, V2, V3} end, DebtsList2),
    {reply, DebtList ++ DebtList2, State};

handle_call(get_debts, _From, State=#state{debts=Debts, users=_Users, debt_record=_DebtRecord}) ->
    DebtList = dets:foldl(fun({{P1,P2}, Amount}, Acc) -> [{P1,P2,Amount}|Acc] end, [], Debts),
    {reply, DebtList, State};

handle_call(get_transactions, _From, State=#state{debts=_Debts, users=_Users, debt_record=Transactions}) ->
    DebtList = dets:foldl(fun({{P1,P2}, Reason, Amount}, Acc) -> [{P1,P2,Reason,Amount}|Acc] end, [], Transactions),
    {reply, DebtList, State};

handle_call(_Request, _From, State=#state{debts=_Debts, users=_Users, debt_record=_DebtRecord}) ->
    io:format("Module: ~p function: handle_call~nState~p~n", [?MODULE, State]),
    {reply, ok, State}.

handle_cast({add, {struct, Struct}}, State=#state{debts=Debts, users=Users, debt_record=DebtRecord}) ->
    %{struct,[{_, P1},{_,P2}, {_,Reason}, {_,Amount}]}
    P1 = proplists:get_value(<<"user1">>, Struct),
    P2 = proplists:get_value(<<"user2">>, Struct),
    Reason = proplists:get_value(<<"reason">>, Struct),
    Amount = proplists:get_value(<<"amount">>, Struct),
    %insert people in to the database at first
    dets:insert(Users, {P1}),
    dets:insert(Users, {P2}),
    dets:insert(DebtRecord, sort_user_debt(P1, P2, Reason, Amount)),
    add_to_earlier_debt(sort_user_debt(P1, P2, Reason, Amount), Debts),
    {noreply, State};

handle_cast(_Msg, State=#state{debts=Debts, users=Users, debt_record=DebtRecord}) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State=#state{debts=Debts, users=Users, debt_record=DebtRecord}) ->
    dets:close(Debts),
    dets:close(Users),
    dets:close(DebtRecord),
    ok.

code_change(OldVsn, State, _Extra) ->
    error_logger:info_msg("UPGRADING VERSION ~n~p~n~p~n",[OldVsn, State]),
    {ok, State}.

sort_user_debt(P1,P2,Reason, Amount) ->
     case P1 < P2 of
          true -> {{P1,P2}, Reason, Amount};
          _ -> {{P2,P1}, Reason, (-1) * Amount}
     end.

%%Fix to be safe
add_to_earlier_debt({Key, _,Amount}, Db) ->
      case dets:lookup(Db, Key) of
           [] -> dets:insert(Db, {Key, Amount});
           [{_,Amount2}] -> dets:insert(Db, {Key, Amount + Amount2})
      end.
