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
handle_call({'DELETE', Ud, [Path, StrId]}, _From, State) ->
    Userdata = users:update_find(Ud),
    Id = list_to_binary(StrId),
    case Path of
        "debts" ->
            pay_server:remove_user_debt(Id,Userdata),
            {reply, ok, State};
        "transactions" ->
            pay_server:delete_debt({delete_debt, Userdata, Id}),
            {reply, ok, State};
        "ios_token" ->
            pay_push_notification:clear_counter(Userdata#user_data.username),
            {reply, ok, State};
        "android_token" ->
            pay_push_notification:remove_android_token(Userdata#user_data.username, Id),
            {reply, ok, State};
        _ -> lager:alert("Unknown path ~p", [Path]),
             {reply, failed, State}
    end;

%%updates
handle_call({'PUT', Ud, Path, Props}, _From, State) ->
    Userdata = users:update_find(Ud),
    case Path of
        ["users"] -> %% currently we only support change username
            pay_server:change_userinfo(Userdata, Props),
            {reply, {ok, [[?STATUS(<<"ok">>)]]}, State};
        ["debts"] -> %% transfer debts
            [{_,OldUid}] = proplists:lookup_all(?OLD_UID, Props),
            [{_,NewUid}] = proplists:lookup_all(?NEW_UID, Props),
            lager:info("transfer_debts OldUid: ~p to NewUid: ~p requested by: ~p~n", [OldUid, NewUid, Userdata]),
            case pay_server:transfer_debts(OldUid, NewUid, Userdata) of
                ok ->
                    {reply, {ok, [[?STATUS(<<"ok">>)]]}, State};
                {nok, Error} ->
                    {reply, {nok, Error}, State}
            end;
        _ -> {reply, {ok, [[?STATUS(<<"ok">>)]]}, State}
    end;

%%creations
handle_call({'POST', Ud, Path, Props}, _From, State) ->
    Userdata = users:update_find(Ud),
    case Path of
        ["users"] -> %%We should return the created user(s)
            lager:info("Creating users: ~p", [Props]),
            UUser = lists:map(fun(EchoAndUser) ->
                                      Res = pay_server:register_user(EchoAndUser, Userdata),
                                      ?JSONSTRUCT([{?USER,Res}])
                              end, Props),
            {reply, {ok, UUser}, State};
        ["transactions"] ->
            Reply = lists:map(fun(EchoAndTransaction) ->
                                      lager:debug("Add transaction: ~p", [EchoAndTransaction]),
                                      Tmp = pay_server:add_debt({add,Userdata, EchoAndTransaction}),
                                      ?JSONSTRUCT([{?TRANSACTION,Tmp}])
                              end, Props),
            {reply, {ok, Reply}, State};
        ["android_token"] ->
            [{_,AndroidToken}] = proplists:lookup_all(?ANDROID_TOKEN, Props),
            pay_push_notification:add_user_android(Userdata#user_data.username, AndroidToken),
            {reply, {ok, [{?ANDROID_TOKEN, AndroidToken}]}, State};
        ["ios_token"] ->
            [{_,IosToken}] = proplists:lookup_all(?IOS_TOKEN, Props),
            pay_push_notification:add_user_ios(Userdata#user_data.username, IosToken),
            {reply, {ok, [{?IOS_TOKEN, IosToken}]}, State};
        _ -> {reply, {ok, [[?STATUS(<<"ok">>)]]}, State}
    end;

%return res
handle_call({'GET', Ud, Path}, _From, State) ->
    case Path of
        ["countries"] ->
            Countries = exchangerates_server:countries(),
            case Ud#user_data.protocol of
                "0.37" ->
                    Structified = lists:map(fun({Short, Long}) ->
                                                    ?JSONSTRUCT([{<<"country">>,
                                                                     [{<<"country_code">>, Short},
                                                                      {<<"country_name">>, Long}]}])
                                            end, Countries),
                    {reply, {ok, Structified}, State};
                _ ->
                    {reply, {ok, Countries}, State}
            end;
        ["country", CountryCode] ->
            {reply, {ok, [exchangerates_server:country(list_to_binary(CountryCode))]}, State};
        ["rates"] ->
            Rates = exchangerates_server:rates(),
            case Ud#user_data.protocol of
                "0.37" ->
                    Structified = lists:map(fun({Short, Rate}) ->
                                                    ?JSONSTRUCT([{<<"exchange_rate">>,
                                                                     [{<<"country_code">>, Short},
                                                                      {<<"rate">>, Rate}]}])
                                            end, Rates),
                    {reply, {ok, Structified}, State};
                _ ->
                    {reply, {ok, Rates}, State}
            end;
        ["crates"] ->
            Rates = exchangerates_server:crates(), %{code, name, rate}
            case Ud#user_data.protocol of
                "0.37" ->
                    Structified = lists:map(fun({Short, Long, Rate}) ->
                                                    ?JSONSTRUCT([{<<"exchange_rate">>,
                                                                     [{<<"country_code">>, Short},
                                                                      {<<"country_name">>, Long},
                                                                      {<<"rate">>, Rate}]}])
                                            end, Rates),
                    {reply, {ok, Structified}, State};
                _ ->
                    {reply, {ok, Rates}, State}
            end;

        ["rate", CountryCode] ->
            {reply, {ok, [exchangerates_server:rate(list_to_binary(CountryCode))]}, State};
        ["users"|Uids] ->
            RealUids = lists:map(fun(Val) -> {?UID, ?UID_TO_LOWER(list_to_binary(Val))} end, Uids),
            Struct = pay_server:get_usernames(RealUids, Ud),
            Ret = lists:map(fun(List) ->
                                    case List of
                                        [{error, E}] -> ?JSONSTRUCT([{error, E}]);
                                        _ ->
                                            ?JSONSTRUCT([?USER(List)])
                                    end
                            end,
                            Struct),
            {reply, {ok, Ret}, State};
        _ ->
            Userdata = users:update_find(Ud),
            case Path of
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
                                      my_sublist(Sorted, F, T)),
                    {reply, {ok, Return}, State};
                ["debts"] ->
                    Return = lists:map(fun(List) ->
                                               ?JSONSTRUCT([?DEBT(List)]) end,
                                       pay_server:get_user_debt(Userdata)),
                    {reply, {ok, Return}, State};
                _ -> {reply, {ok, <<"ok">>}, State}
            end
    end;

handle_call(Request, _From, State) ->
    lager:alert("Handle unknown call ~p", [Request]),
    {reply, {ok, [[?STATUS(<<"ok">>)]]}, State}.

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

my_sublist(List,From,To) ->
    F = From + 1, %1 indexed
    case F > length(List) of
        true ->
            [];
        _ ->
            lists:sublist(List, F, To +1 - From)
    end.
