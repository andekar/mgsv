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

%% callbacks
%handle_call([], _From, State) ->
%    {reply, {ok, "ok"}, State};

handle_call(Struct = [{?JSONSTRUCT, [{?DEBT, _Vars}]}|_Debts], _From, State) ->
    error_logger:info_msg("Adding debtjson ~n~p~n",[Struct]),
    Reply = lists:map(fun({?JSONSTRUCT, [{?DEBT, Vars}]}) ->
                               error_logger:info_msg("Adding debt~n~p~n",[Vars]),
                               Tmp = pay_server:add_debt({add,Vars}),
                               ?JSONSTRUCT([{?DEBT,Tmp}])
                               end, Struct),
    {reply, {ok, mochijson2:encode(Reply)}, State};

handle_call(Uids = [{?JSONSTRUCT, [{?UID, _Vars}]}|_Uids], _From, State) ->
    RealUids = lists:map(fun({?JSONSTRUCT, [Val]}) -> Val end, Uids),
    error_logger:info_msg("Requesting usernames ~n~p~n",[RealUids]),
    Struct = pay_server:get_usernames(RealUids),
    {reply, {ok, mochijson2:encode(Struct)}, State};

handle_call(["users"], _From, State) ->
    Return = lists:map(fun({Uuid, User}) ->
                       ?JSONSTRUCT([?UID(Uuid), ?USER(User)]) end,
                       pay_server:call_pay(get_users)),
    {reply, {ok, mochijson2:encode(Return)}, State};

handle_call(["debts"], _From, State) ->
    Return = lists:map(fun({P1,P2,Amount}) ->
                           ?JSONSTRUCT([?DEBT([ ?UID1(P1)
                                              , ?UID2(P2)
                                              , ?AMOUNT(Amount)
                                                              ])]) end,
                                                     pay_server:get_debts()),
    Return2 = mochijson2:encode(Return),
    {reply, {ok, Return2}, State};

handle_call(["user_debt", User], _From, State) ->
    Return = lists:map(fun({P1,P2,Amount}) ->
                           ?JSONSTRUCT([?DEBT([ ?UID1(P1)
                                              , ?UID2(P2)
                                              , ?AMOUNT(Amount)
                                                              ])]) end,
                                                     pay_server:get_user_debt(list_to_binary(User))),
    Return2 = mochijson2:encode(Return),
    {reply, {ok, Return2}, State};

handle_call(["user_transactions", User], _From, State) ->
    Return = lists:map(fun({Uuid, P1, P2, TimeStamp, Reason, Amount}) ->
                           ?JSONSTRUCT([?DEBT([ ?UUID(Uuid)
                                              , ?UID1(P1)
                                              , ?UID2(P2)
                                              , ?TIMESTAMP(TimeStamp)
                                              , ?REASON(Reason)
                                              , ?AMOUNT(Amount)
                                                              ])]) end,
                                                     pay_server:get_user_transactions(list_to_binary(User))),
    Return2 = mochijson2:encode(Return),
    {reply, {ok, Return2}, State};

handle_call(["transactions"], _From, State) ->
    Return = lists:map(fun({Uuid, P1, P2, TimeStamp, Reason, Amount}) ->
                           ?JSONSTRUCT([?DEBT([ ?UUID(Uuid)
                                              , ?UID1(P1)
                                              , ?UID2(P2)
                                              , ?TIMESTAMP(TimeStamp)
                                              , ?REASON(Reason)
                                              , ?AMOUNT(Amount)
                                                              ])]) end,
                                                     pay_server:get_transactions()),
    Return2 = mochijson2:encode(Return),
    {reply, {ok, Return2}, State};

handle_call(_Request, _From, State) ->
    error_logger:info_msg("Error no Matches~n",[]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

