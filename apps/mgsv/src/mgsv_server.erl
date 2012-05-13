-module(mgsv_server).

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

handle_call(["add", P1, P2, Reason, Amount], _From, State) ->
    Add = {P1, P2, Reason, Amount},
    pay_server:cast_pay({add, P1, P2, Reason, Amount}),
    {reply, {ok, "ok"}, State};

handle_call(["users"], _From, State) ->
    Return = lists:map(fun({User}) -> {struct,[{user,User}]} end, pay_server:call_pay(get_users)),
    {reply, {ok, mochijson2:encode(Return)}, State};

handle_call(["get"], _From, State) ->
    Return = lists:map(fun({P1,P2,Amount}) ->
                           {struct, [{debt, {struct,[{user1, P1},
                                                    {user2, P2},
                                                    {amount, Amount}
                                                              ]}}]} end,
                                                     pay_server:get_debts()),
    Return2 = mochijson2:encode(Return),
    {reply, {ok, Return2}, State};

handle_call(["get_transactions"], _From, State) ->
    Return = lists:map(fun({P1,P2,Reason, Amount}) ->
                           {struct, [{debt, {struct,[{user1, P1},
                                                    {user2, P2},
                                                    {reason, Reason},
                                                    {amount, Amount}
                                                              ]}}]} end,
                                                     pay_server:get_transactions()),
    Return2 = mochijson2:encode(Return),
    {reply, {ok, Return2}, State};

handle_call(_Request, _From, State) ->
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

