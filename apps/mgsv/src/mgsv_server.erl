-module(mgsv_server).

-include("payapp.hrl").
-include("common.hrl").

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
handle_call({'DELETE', Ud, [Path, StrId], https}, _From, State) ->
    Userdata = users:update_find(Ud),
    Id = list_to_binary(StrId),
    case Path of
        "debts" ->
            pay_server:remove_user_debt(Id,Userdata),
            {reply, ok, State};
        "transactions" ->
            pay_server:delete_debt({delete_debt, Userdata, Id}),
            {reply, ok, State};
        "feedback" ->
            pay_server:remove_feedback(Id),
            {reply, ok, State};
        "ios_token" ->
            pay_push_notification:clear_counter(Userdata#user_data.username),
            {reply, ok, State};
        _ -> lager:alert("Unknown path ~p", [Path]),
             {reply, failed, State}
    end;

%%updates
handle_call({'PUT', Ud, Path, Props, https}, _From, State) ->
    Userdata = users:update_find(Ud),
    case Path of
        ["users"] -> %% currently we only support change username
            pay_server:change_userinfo(Userdata, Props),
            {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State};
        ["debts"] -> %% transfer debts
            [{_,OldUid}] = proplists:lookup_all(?OLD_UID, Props),
            [{_,NewUid}] = proplists:lookup_all(?NEW_UID, Props),
            lager:info("transfer_debts OldUid: ~p to NewUid: ~p requested by: ~p~n", [OldUid, NewUid, Userdata]),
            pay_server:transfer_debts(OldUid, NewUid, Userdata),
            {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State};
        _ -> {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State}
    end;

%%creations
handle_call({'POST', Ud, Path, Props, https}, _From, State) ->
    Userdata = users:update_find(Ud),
    case Path of
        ["users"] -> %%We should return the created user(s)
            lager:info("Creating users: ~p", [Props]),
            Users = proplists:lookup_all(?USER, Props),
            UUsers = case Users of
                         [{?USER, MaybeList} | _MaybeMore] when is_list(MaybeList)-> Users;
                         _    -> [{?USER, Props}]
                     end,
            UUser = lists:map(fun({?USER, User}) ->
                                      Res = pay_server:register_user(User, Userdata),
                                      ?JSONSTRUCT([{?USER,Res}])
                              end, UUsers),
            {reply, {ok, mochijson2:encode(UUser)}, State};
        ["transactions"] ->
            Transactions = proplists:lookup_all(?TRANSACTION, Props),
            Reply = lists:map(fun({?TRANSACTION, Vars}) ->
                                      lager:debug("Add debt: ~p", [Vars]),
                                      Tmp = pay_server:add_debt({add,Userdata, Vars}),
                                      ?JSONSTRUCT([{?TRANSACTION,Tmp}])
                              end, Transactions),
            {reply, {ok, mochijson2:encode(Reply)}, State};
        ["feedback"] ->
            [{?FEEDBACK, Feedback}] = proplists:lookup_all(?FEEDBACK, Props),
            {ok, Result} = pay_server:add_feedback(Userdata#user_data.username, Feedback),
            {reply, {ok, mochijson2:encode([{?FEEDBACK, Result}])}, State};
        ["android_token"] ->
            [{_,AndroidToken}] = proplists:lookup_all(?ANDROID_TOKEN, Props),
            pay_push_notification:add_user_android(Userdata#user_data.username, AndroidToken),
            {reply, {ok, mochijson2:encode([{?ANDROID_TOKEN, AndroidToken}])}, State};
        ["ios_token"] ->
            [{_,IosToken}] = proplists:lookup_all(?IOS_TOKEN, Props),
            pay_push_notification:add_user_ios(Userdata#user_data.username, IosToken),
            {reply, {ok, mochijson2:encode([{?IOS_TOKEN, IosToken}])}, State};
        _ -> {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State}
    end;

%return res
handle_call({'GET', Ud, Path, https}, _From, State) ->
    Userdata = users:update_find(Ud),
    case Path of
        ["countries"] ->
            {reply, {ok, mochijson2:encode(exchangerates_server:countries())}, State};
        ["country", CountryCode] ->
            {reply, {ok, mochijson2:encode([exchangerates_server:country(list_to_binary(CountryCode))])}, State};
        ["rates"] ->
            {reply, {ok, mochijson2:encode(exchangerates_server:rates())}, State};
        ["rate", CountryCode] ->
            {reply, {ok, mochijson2:encode([exchangerates_server:rate(list_to_binary(CountryCode))])}, State};
        ["users"|Uids] ->
            RealUids = lists:map(fun(Val) -> {?UID, ?UID_TO_LOWER(list_to_binary(Val))} end, Uids),
            Struct = pay_server:get_usernames(RealUids, Userdata),
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
            Transactions = pay_server:get_user_transactions(Userdata),
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
                                                    lists:any(fun({?UID1, TBinUid}) ->
                                                                      TBinUid =:= BinUid;
                                                                 ({?UID2, TBinUid}) ->
                                                                      TBinUid =:= BinUid;
                                                                 ({<<"paid_by">>, TBinUid}) ->
                                                                      TBinUid =:= BinUid;
                                                                 ({<<"paid_for">>, TBinUid}) ->
                                                                      TBinUid =:= BinUid;

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
            Return = lists:map(fun(List) ->
                                       ?JSONSTRUCT([?DEBT(List)]) end,
                               pay_server:get_user_debt(Userdata)),
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

handle_call(Request, _From, State) ->
    lager:alert("Handle unknown call ~p", [Request]),
    {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State}.

handle_cast(Request, State) ->
    lager:alert("Handle unknown cast ~p", [Request]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    lager:emergency("TERMINATING ~p~n~p", [Reason,erlang:get_stacktrace()]),
    ok.

code_change(OldVsn, State, Extra) ->
    lager:debug("UPGRADING VERSION ~n~p~n~p~n~p~n",[OldVsn, State, Extra]),
    {ok, State}.
