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

%Add debts
handle_call({[], Struct, _Scheme}, _From, State) ->
    [{_, ReqBy}] = proplists:lookup_all(?REQUEST_BY, Struct),
    Reply = lists:map(fun({?DEBT, Vars}) ->
                              lager:debug("Add debt: ~p", [Vars]),
                               Tmp = pay_server:add_debt({add,ReqBy, Vars}),
                               ?JSONSTRUCT([{?DEBT,Tmp}])
                               end, proplists:delete(?REQUEST_BY, Struct)),
    {reply, {ok, mochijson2:encode(Reply)}, State};

%get users
handle_call({["users"], Uids, _Scheme}, _From, State) ->
    RealUids = lists:map(fun(Val) -> Val end, Uids),
    Struct = pay_server:get_usernames(proplists:delete(?REQUEST_BY, RealUids)),
    {reply, {ok, mochijson2:encode(Struct)}, State};

%register user
handle_call({["register"], Struct, _Scheme}, _From, State) ->
    pay_server:register_user(Struct),
    {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State};

%delete debt
handle_call({["delete_debt"], Struct, _Scheme}, _From, State) ->
    [{_,ReqBy}] = proplists:lookup_all(?REQUEST_BY, Struct),
    Uuids = proplists:lookup_all(?UUID, Struct),
    _Reply = lists:map(fun({?UUID, Vars}) ->
                               lager:info("delete_debt: ~p", [Vars]),
                               pay_server:delete_debt({delete_debt, ReqBy, Vars})
                       end, Uuids),
    {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State};

handle_call({["delete_user_debt"], Struct, _Scheme}, _From, State) ->
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

handle_call({["users"], _Scheme}, _From, State) ->
    Return = lists:map(fun(PropList) ->
                       ?JSONSTRUCT(PropList) end,
                       pay_server:call_pay(get_users)),
    {reply, {ok, mochijson2:encode(Return)}, State};

handle_call({["user_debt", User], _Scheme}, _From, State) ->
    Return = lists:map(fun({_Key,List}) ->
                               ?JSONSTRUCT([?DEBT(List)]) end,
                       pay_server:get_user_debt(list_to_binary(User))),
    Return2 = mochijson2:encode(Return),
    {reply, {ok, Return2}, State};

handle_call({["user_transactions", User], _Scheme}, _From, State) ->
    Return = lists:map(fun(List) ->
                               ?JSONSTRUCT([?DEBT(List)]) end,
                       pay_server:get_user_transactions(list_to_binary(User))),
    Return2 = mochijson2:encode(Return),
    {reply, {ok, Return2}, State};

handle_call({["user_transactions", User, Num], _Scheme}, _From, State) ->
    Transactions = pay_server:get_user_transactions(list_to_binary(User)),
    Sorted = lists:sort(fun(T1,T2) ->
                                DT1 = proplists:get_value(?SERVER_TIMESTAMP, T1),
                                DT2 = proplists:get_value(?SERVER_TIMESTAMP, T2),
                                DT1 =< DT2 end,
                        Transactions),
    {PNum, []} = string:to_integer(Num),
    Return = lists:map(fun(List) ->
                               ?JSONSTRUCT([?DEBT(List)]) end,
                       lists:sublist(Sorted, PNum)),
    Return2 = mochijson2:encode(Return),
    {reply, {ok, Return2}, State};

handle_call({["username"], Struct, https}, _From, State) ->
    [{_, UserId}] = proplists:lookup_all(?UID, Struct),
    [{_, UserName}] = proplists:lookup_all(?USER, Struct),
    pay_server:change_username(UserId, UserName),
    {reply, {ok, mochijson2:encode([[?STATUS(<<"ok">>)]])}, State};

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
