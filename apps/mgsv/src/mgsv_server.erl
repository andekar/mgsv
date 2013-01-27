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

%Add debts
handle_call({[], TStruct}, _From, State) ->
    %destructify
    Struct = lists:flatten(destructify_list(TStruct)),
    [{_, ReqBy}] = proplists:lookup_all(?REQUEST_BY, Struct),
    Reply = lists:map(fun({?DEBT, Vars}) ->
                              lager:debug("Add debt: ~p", [Vars]),
                               Tmp = pay_server:add_debt({add,ReqBy, Vars}),
                               ?JSONSTRUCT([{?DEBT,Tmp}])
                               end, proplists:delete(?REQUEST_BY, Struct)),
    {reply, {ok, mochijson2:encode(Reply)}, State};

%get users
handle_call({["users"], Uids}, _From, State) ->
    RealUids = lists:map(fun({?JSONSTRUCT, [Val]}) -> Val end, Uids),
    Struct = pay_server:get_usernames(proplists:delete(?REQUEST_BY, RealUids)),
    {reply, {ok, mochijson2:encode(Struct)}, State};

%register user
handle_call({["register"], TStruct}, _From, State) ->
    %destructify
    Struct = lists:flatten(destructify_list(TStruct)),
    [{_, Uid}] = proplists:lookup_all(?UID, Struct),
    [{_, Name}] = proplists:lookup_all(<<"name">>, Struct),
    lager:info("Registering user ~p with uid ~p", [Name, Uid]),
    pay_server:register_user(Name, Uid),
    {reply, {ok, mochijson2:encode(<<"ok">>)}, State};


%delete debt
handle_call({["delete_debt"], TStruct}, _From, State) ->
    %destructify
    Struct = lists:flatten(destructify_list(TStruct)),
    [{_,ReqBy}] = proplists:lookup_all(?REQUEST_BY, Struct),
    _Reply = lists:map(fun({?UUID, Vars}) ->
                               lager:info("Deleting: ~p", [Vars]),
                               pay_server:delete_debt({delete_debt, ReqBy, Vars})
                               end, proplists:delete(?REQUEST_BY, Struct)),
    {reply, {ok, mochijson2:encode(<<"ok">>)}, State};

%delete debt
handle_call({["transfer_debts"], TStruct}, _From, State) ->
    %destructify
    Struct = lists:flatten(destructify_list(TStruct)),
    [{_,ReqBy}]  = proplists:lookup_all(?REQUEST_BY, Struct),
    [{_,OldUid}] = proplists:lookup_all(?OLD_UID, Struct),
    [{_,NewUid}] = proplists:lookup_all(?NEW_UID, Struct),
    lager:info("transfer debt from OldUid: ~p to NewUid: ~p requested by: ~p~n", [OldUid, NewUid, ReqBy]),
    pay_server:transfer_debts(OldUid, NewUid, ReqBy),
    {reply, {ok, mochijson2:encode(<<"ok">>)}, State};

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

handle_call(Request, _From, State) ->
    lager:alert("Handle unknown call ~p", Request),
    {reply, ok, State}.

handle_cast(Request, State) ->
    lager:alert("Handle unknown cast ~p", Request),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    lager:emergency("TERMINATING ~p", Reason),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

destructify_list(List) ->
    lists:map(fun destructify/1, List).

destructify({?JSONSTRUCT, Val}) ->
    Val.
