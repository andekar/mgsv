-module(mgsv_server).

-include("payapp.hrl").

-behaviour(gen_server).

-export([start_link/0, send_message/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

send_message(Message) ->
    gen_server:call(?MODULE, Message).

%% delete requests
handle_call({'DELETE', ReqBy, [Path, StrId], https}, _From, State) ->
    Id = list_to_binary(StrId),
    case Path of
        "debts" ->
            pay_server:remove_user_debt(Id,ReqBy),
            {reply, ok, State};
        "transactions" ->
%            lager:info("delete transaction: ~p", [Id]),
            pay_server:delete_debt({delete_debt, ReqBy, Id}),
            {reply, ok, State};
        "feedback" ->
            pay_server:remove_feedback(Id),
            {reply, ok, State};
        "ios_token" ->
            pay_push_notification:clear_counter(ReqBy),
            {reply, ok, State};
        _ -> lager:alert("Unknown path ~p", [Path]),
             {reply, failed, State}
    end;

%%updates
handle_call({'PUT', ReqBy, Path, Props, https}, _From, State) ->
    case Path of
        ["users"] -> %% currently we only support change username
            pay_server:change_userinfo(ReqBy, Props),
            {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State};
        ["debts"] -> %% transfer debts
            [{_,OldUid}] = proplists:lookup_all(?OLD_UID, Props),
            [{_,NewUid}] = proplists:lookup_all(?NEW_UID, Props),
            lager:info("transfer_debts OldUid: ~p to NewUid: ~p requested by: ~p~n", [OldUid, NewUid, ReqBy]),
            pay_server:transfer_debts(OldUid, NewUid, ReqBy),
            {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State};
        _ -> {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State}
    end;

%%creations
handle_call({'POST', ReqBy, Path, Props, https}, _From, State) ->
    case Path of
        ["users"] -> %%We should return the created user(s)
            User = pay_server:register_user(Props),
            {reply, {ok, mochijson2:encode([?JSONSTRUCT([{?USER, User}])])}, State};
        ["transactions"] ->
            Transactions = proplists:lookup_all(?TRANSACTION, Props),
            Reply = lists:map(fun({?TRANSACTION, Vars}) ->
                                      lager:debug("Add debt: ~p", [Vars]),
                                      Tmp = pay_server:add_debt({add,ReqBy, Vars}),
                                      ?JSONSTRUCT([{?TRANSACTION,Tmp}])
                              end, Transactions),
            {reply, {ok, mochijson2:encode(Reply)}, State};
        ["feedback"] ->
            [{?FEEDBACK, Feedback}] = proplists:lookup_all(?FEEDBACK, Props),
            {ok, Result} = pay_server:add_feedback(ReqBy, Feedback),
            {reply, {ok, mochijson2:encode([{?FEEDBACK, Result}])}, State};
        ["ios_token"] ->
            [{_,IosToken}] = proplists:lookup_all(?IOS_TOKEN, Props),
            pay_push_notification:add_user_ios(ReqBy, IosToken),
            {reply, {ok, mochijson2:encode([{?IOS_TOKEN, IosToken}])}, State};
        _ -> {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State}
    end;

%return res
handle_call({'GET', ReqBy, Path, https}, _From, State) ->
    case Path of
        ["users"|Uids] ->
            RealUids = lists:map(fun(Val) -> {?UID, list_to_binary(Val)} end, Uids),
            Struct = pay_server:get_usernames(RealUids),
            Ret = lists:map(fun(List) ->
                                    case List of
                                        [{error, E}] -> ?JSONSTRUCT([{error, E}]);
                                        _ ->
                                            ?JSONSTRUCT([?USER(List)])
                                    end
                            end,
                            Struct),
            {reply, {ok, mochijson2:encode(Ret)}, State};
        ["transactions"|More] ->
            Transactions = pay_server:get_user_transactions(ReqBy),
            {OtherUid, F, T} = case More of
                         [To] ->
                             {PNum,   []} = string:to_integer(To),
                             {undefined, 0, PNum};
                         [From, To] ->
                             {PFrom, []} = string:to_integer(From),
                             {PNum,   []} = string:to_integer(To),
                             {undefined, PFrom, PNum};
                         [OtherU, From, To] ->
                             {PFrom, []} = string:to_integer(From),
                             {PNum,   []} = string:to_integer(To),
                             {OtherU, PFrom, PNum};
                         _ -> {undefined, 0, 999} %% max num seems almost to large...
                     end,

            Filtered = case OtherUid of
                           undefined -> Transactions;
                           _ ->
                               BinUid = list_to_binary(OtherUid),
                               lists:filter(fun(Props) ->
                                                    lists:any(fun({?UID1, TBinUid}) -> TBinUid =:= BinUid;
                                                                 ({?UID2, TBinUid}) -> TBinUid =:= BinUid;
                                                                 (_) -> false
                                                              end, Props)
                                            end,
                                            Transactions)

                       end,

            Sorted = lists:sort(fun(T1,T2) ->
                                        DT1 = proplists:get_value(?SERVER_TIMESTAMP, T1),
                                        DT2 = proplists:get_value(?SERVER_TIMESTAMP, T2),
                                        DT1 >= DT2 end,
                                Filtered),
            Return = lists:map(fun(List) ->
                                       ?JSONSTRUCT([?TRANSACTION(List)]) end,
                               lists:sublist(lists:nthtail(F, Sorted), T)),
            Return2 = mochijson2:encode(Return),
            {reply, {ok, Return2}, State};
        ["debts"] ->
            Return = lists:map(fun({_Key,List}) ->
                                       ?JSONSTRUCT([?DEBT(List)]) end,
                               pay_server:get_user_debt(ReqBy)),
            Return2 = mochijson2:encode(Return),
            {reply, {ok, Return2}, State};
        ["feedback"] ->
            Res = pay_server:get_feedback(), %% TODO maybe restrict to some users
            Return = lists:map( fun(List) ->
                                        ?JSONSTRUCT([{?FEEDBACK, List}]) end,
                                Res),
            Return2 = mochijson2:encode(Return),
            {reply, {ok, Return2}, State};
        _ -> {reply, {ok, <<"ok">>}, State}
    end;

%%
%Add debts
handle_call({[], Struct, http}, _From, State) ->
    [{_, ReqBy}] = proplists:lookup_all(?REQUEST_BY, Struct),
    Debts = proplists:lookup_all(?DEBT, Struct),
    Reply = lists:map(fun({?DEBT, Vars}) ->
                              lager:debug("Add debt: ~p", [Vars]),
                               Tmp = pay_server:add_debt({add,ReqBy, Vars}),
                               ?JSONSTRUCT([{?DEBT,Tmp}])
                               end, Debts),
    {reply, {ok, mochijson2:encode(Reply)}, State};

%get users
handle_call({["users"], Uids, http}, _From, State) ->
    RealUids = lists:map(fun(Val) -> Val end, Uids),
    Struct = pay_server:get_usernames(proplists:delete(?REQUEST_BY, RealUids)),
    {reply, {ok, mochijson2:encode(Struct)}, State};

%register user
handle_call({["register"], Struct, http}, _From, State) ->
    pay_server:register_user(Struct),
    {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State};

%delete debt
handle_call({["delete_debt"], Struct, http}, _From, State) ->
    [{_,ReqBy}] = proplists:lookup_all(?REQUEST_BY, Struct),
    Uuids = proplists:lookup_all(?UUID, Struct),
    _Reply = lists:map(fun({?UUID, Vars}) ->
                               lager:info("delete_debt: ~p", [Vars]),
                               pay_server:delete_debt({delete_debt, ReqBy, Vars})
                       end, Uuids),
    {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State};

handle_call({["delete_user_debt"], Struct, http}, _From, State) ->
    [{_,ReqBy}] = proplists:lookup_all(?REQUEST_BY, Struct),
    [{_,Uid}] = proplists:lookup_all(?UID, Struct),
    pay_server:remove_user_debt(Uid,ReqBy),
    {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State};

handle_call({["transfer_debts"], Struct, https}, _From, State) ->
    [{_,ReqBy}]  = proplists:lookup_all(?REQUEST_BY, Struct),
    [{_,OldUid}] = proplists:lookup_all(?OLD_UID, Struct),
    [{_,NewUid}] = proplists:lookup_all(?NEW_UID, Struct),
    lager:info("transfer_debts OldUid: ~p to NewUid: ~p requested by: ~p~n", [OldUid, NewUid, ReqBy]),
    pay_server:transfer_debts(OldUid, NewUid, ReqBy),
    {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State};

handle_call({["users"], http}, _From, State) ->
    Return = lists:map(fun(PropList) ->
                       ?JSONSTRUCT(PropList) end,
                       pay_server:call_pay(get_users)),
    {reply, {ok, mochijson2:encode(Return)}, State};

handle_call({["user_debt", User], http}, _From, State) ->
    Return = lists:map(fun({_Key,List}) ->
                               ?JSONSTRUCT([?DEBT(List)]) end,
                       pay_server:get_user_debt(list_to_binary(User))),
    Return2 = mochijson2:encode(Return),
    {reply, {ok, Return2}, State};

handle_call({["user_transactions", User], http}, _From, State) ->
    Return = lists:map(fun(List) ->
                               ?JSONSTRUCT([?DEBT(List)]) end,
                       pay_server:get_user_transactions(list_to_binary(User))),
    Return2 = mochijson2:encode(Return),
    {reply, {ok, Return2}, State};

handle_call({["user_transactions", User, Num], http}, _From, State) ->
    Transactions = pay_server:get_user_transactions(list_to_binary(User)),
    Sorted = lists:sort(fun(T1,T2) ->
                                DT1 = proplists:get_value(?SERVER_TIMESTAMP, T1),
                                DT2 = proplists:get_value(?SERVER_TIMESTAMP, T2),
                                DT1 >= DT2 end,
                        Transactions),
    {PNum, []} = string:to_integer(Num),
    Return = lists:map(fun(List) ->
                               ?JSONSTRUCT([?DEBT(List)]) end,
                       lists:sublist(Sorted, PNum)),
    Return2 = mochijson2:encode(Return),
    {reply, {ok, Return2}, State};

handle_call({["user_transactions", User, From, Num], http}, _From, State) ->
    Transactions = pay_server:get_user_transactions(list_to_binary(User)),
    Sorted = lists:sort(fun(T1,T2) ->
                                DT1 = proplists:get_value(?SERVER_TIMESTAMP, T1),
                                DT2 = proplists:get_value(?SERVER_TIMESTAMP, T2),
                                DT1 >= DT2 end,
                        Transactions),
    {PFrom, []} = string:to_integer(From),
    {PNum,   []} = string:to_integer(Num),
    Return = lists:map(fun(List) ->
                               ?JSONSTRUCT([?DEBT(List)]) end,
                       lists:sublist(lists:nthtail(PFrom, Sorted), PNum)),
    Return2 = mochijson2:encode(Return),
    {reply, {ok, Return2}, State};

handle_call({["feedback"], Struct, _ANY}, _From, State) ->
    [{_,ReqBy}]  = proplists:lookup_all(?REQUEST_BY, Struct),
    case  proplists:lookup_all(?FEEDBACK, Struct) of
        [{_, Feedback}] ->
            pay_server:add_feedback(ReqBy, Feedback),
            {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State};
        [] -> case proplists:lookup(?UUID, Struct) of
                  [{_, Uuid}] -> pay_server:remove_feedback(Uuid),
                                 {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State};
                  undefined ->
                      Res = pay_server:get_feedback(), %% TODO maybe restrict to some users
                      Return = lists:map( fun(List) ->
                                                  ?JSONSTRUCT([{?FEEDBACK, List}]) end,
                                          Res),
                      Return2 = mochijson2:encode(Return),
                      {reply, {ok, Return2}, State}
              end
    end;

handle_call({["ios_token"], Struct, https}, _From, State) ->
    [{_,ReqBy}]  = proplists:lookup_all(?REQUEST_BY, Struct),
    [{_,IosToken}] = proplists:lookup_all(?IOS_TOKEN, Struct),
    pay_push_notification:add_user_ios(ReqBy, IosToken),
    {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State};

handle_call({["clear_badges", User], https}, _From, State) ->
    pay_push_notification:clear_counter(list_to_binary(string:to_lower(User))),
    {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State};

handle_call(Request, _From, State) ->
    lager:alert("Handle unknown call ~p", [Request]),
    {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State}.

handle_cast(Request, State) ->
    lager:alert("Handle unknown cast ~p", [Request]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    lager:emergency("TERMINATING ~p", [Reason]),
    ok.

code_change(OldVsn, State, Extra) ->
    lager:debug("UPGRADING VERSION ~n~p~n~p~n~p~n",[OldVsn, State, Extra]),
    {ok, State}.
