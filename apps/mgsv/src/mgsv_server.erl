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

handle_call(["add", P1, P2, Amount], _From, State) ->
    Add = {P1, P2, Amount},
    {reply, {ok, "ok"}, [Add | State]};

handle_call(["users"], _From, State) ->
    {reply, {ok, "ok"}, State};

handle_call(["get"], _From, State) ->
    Return = lists:map(fun({P1,P2,Amount}) ->
                           {struct, [{debt, {struct,[{user1,list_to_binary(P1)},
                                                    {user2,list_to_binary(P2)},
                                                    {amount, list_to_integer(Amount)}
                                                              ]}}]} end, State),
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

