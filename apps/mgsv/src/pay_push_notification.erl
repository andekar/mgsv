-module(pay_push_notification).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([add_user_ios/2,
         add_user_android/2,
         remove_user_ios/2,
         notify_user/2,
         clear_counter/1,
         print_users/0,
         remove_ios_token/1,
         remove_android_token/2]).

-record(state, {
          ios,
          android
         }).

start_link() ->
    AndroidId =
        case application:get_env(push_android, id) of
            undefined ->
                undefined; %crash
            {ok, Val} ->
                Val
        end,
    lager:info("Starting pay_push_notification"),
    apns:connect(payapp, fun(Ab,Ba) -> error_logger:error_msg("Error ~p ~p", [Ab,Ba]) end, fun(Arg) -> error_logger:info_msg("error ~p~n~n",[Arg]), remove_ios_token(Arg) end),
    gcm:start(android,AndroidId),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {_,E} = db_w:open_file("../../ios_push_not_0.3.2.dets",[{type, bag}]),
    {_,F} = db_w:open_file("../../android_push_not_0.3.2.dets",[{type, bag}]),
    {ok, #state{ios = E,
                android = F}}.

print_users() ->
    gen_server:call(?MODULE, print_users).

add_user_ios(Uuid, DevId) ->
    gen_server:cast(?MODULE, {add_user_ios, Uuid, DevId}).

add_user_android(Uuid, DevId) when is_list(DevId) ->
    add_user_android(Uuid,list_to_binary(DevId));
add_user_android(Uuid, DevId) ->
    gen_server:cast(?MODULE, {add_user_android, Uuid, DevId}).

remove_user_ios(Uuid, DevId) ->
    gen_server:cast(?MODULE, {remove_user_ios, Uuid, DevId}).

clear_counter(Uuid) ->
    lager:info("clear counter for ~p", [Uuid]),
    gen_server:cast(?MODULE, {clear_counter, Uuid}).

notify_user(Uuid, Message) ->
    gen_server:cast(?MODULE, {notify_user, Uuid, Message}).

remove_ios_token(Token) ->
    gen_server:cast(?MODULE, {remove_ios_token, Token}).

remove_android_token(Uuid, DevId) ->
    gen_server:cast(?MODULE, {remove_android_token, Uuid, DevId}).

handle_call(print_users, _From, #state {ios = IOS,
                                        android = Android} = State) ->
    dets:traverse(IOS,
                  fun({Uuid, DevTok, Count}) ->
                          lager:info("Uuid ~p DevTok ~p Count ~p", [Uuid, DevTok, Count]),
                          continue
                  end),
    dets:traverse(Android,
                  fun({Uuid, DevTok}) ->
                          lager:info("Uuid ~p DevTok ~p", [Uuid, DevTok]),
                          continue
                  end),
    {reply, ok, State};

handle_call(All, _From, State) ->
    lager:debug("unhandled handle_call ~p", [All]),
    {reply, ok, State}.

handle_cast({add_user_ios, Uuid, DevId}, #state{ios = IOS,
                                                android = _Android} = State) ->
    lager:info("~p Adding ios user ~p with UUid ~p",[?MODULE, Uuid, DevId]),
    case dets:lookup(IOS, Uuid) of
        Any when is_list(Any) ->
            case lists:any(fun({_Uuid, DevTok, _Count}) -> DevTok == DevId end, Any) of
                false -> dets:insert(IOS, {Uuid, binary_to_list(DevId), 0});
                _ -> ok
            end;
        _ -> ok
    end,
    {noreply, State};

handle_cast({add_user_android, Uuid, DevId}, #state{ios = _IOS,
                                                    android = Android} = State) ->
    lager:info("~p Adding android user ~p with UUid ~p", [?MODULE, Uuid, DevId]),
    case dets:lookup(Android, Uuid) of
        Any when is_list(Any) ->
            case lists:any(fun({_Uuid, DevTok}) -> DevTok == DevId end, Any) of
                false -> dets:insert(Android, {Uuid, binary_to_list(DevId)});
                _ -> ok
            end;
        _ -> ok
    end,
    {noreply, State};

handle_cast({notify_user, Uuid, Message}, State) when is_list(Message) ->
    handle_cast({notify_user, Uuid, list_to_binary(Message)}, State);

handle_cast({notify_user, Uuid, Message}, State) when is_list(Uuid) ->
    handle_cast({notify_user, list_to_binary(Uuid), Message}, State);

handle_cast({notify_user, Uuid, Message}, #state{ios = IOS,
                                                 android = Android} = State) ->
    AndroidMessage = [{<<"data">>, [
                                    {<<"message">>, Message}
                                   ]}],
    case dets:lookup(IOS, Uuid) of
        Any when is_list(Any) ->
            lists:map(fun({_Uuid, DevTok, Count}) ->
                              try
                                  lager:info("devicetoken ~p message ~p count ~p Uuid ~p", [DevTok, binary_to_list(Message), Count, Uuid]),
                                  apns:send_message(payapp, DevTok, Message, Count + 1),
                                  dets:delete_object(IOS, {Uuid, DevTok, Count}),
                                  dets:insert(IOS, {Uuid, DevTok, Count + 1})
                              catch
                                  _:_ -> dets:delete_object(IOS, {Uuid, DevTok, Count})
                              end
                      end, Any);
        _ -> ok
    end,
    case dets:lookup(Android, Uuid) of
        Any2 when is_list(Any2) ->
            lists:map(fun({_Uuid, DevTok}) ->
                              lager:info("devicetoken ~p message ~p Uuid ~p", [DevTok, AndroidMessage, Uuid]),
                              DTok = case is_list(DevTok) of
                                         true ->
                                             list_to_binary(DevTok);
                                         _ -> DevTok
                                     end,
                              gcm:push(android, [DTok], AndroidMessage)
                      end, Any2);
        _ -> ok

    end,
    {noreply, State};

handle_cast({clear_counter, Uuid}, #state{ios = IOS,
                                          android = _Android} = State) ->
    case dets:lookup(IOS, Uuid) of
        Any when is_list(Any) ->
            lists:map(fun({_Uuid, DevTok, Count}) ->
                              lager:info("clear count devicetoken ~p count ~p uuid ~p", [DevTok, Count, Uuid]),
                              try
                                  apns:send_badge(payapp, DevTok, 0),
                                  dets:delete_object(IOS, {Uuid, DevTok, Count}),
                                  dets:insert(IOS, {Uuid, DevTok, 0})
                              catch
                                  _:_ -> dets:delete_object(IOS, {Uuid, DevTok, Count})
                              end
                      end, Any),
            {noreply, State};
        _ -> {norely, State}
    end;

handle_cast({remove_user_ios, Uuid, DevId}, #state{ios = IOS,
                                                   android = _Android} = State) ->
    lager:info("Deleting user ~p with UUid ~p",[Uuid, DevId]),
    dets:delete_object(IOS, {Uuid, binary_to_list(DevId), 0}),
    {noreply, State};

handle_cast({remove_ios_token, Token}, #state{ios = IOS,
                                              android = _Android} = State) ->
    List = dets:foldl(fun({Uuid, DevTok, Count}, Acc) ->
                       case DevTok == Token of
                           true -> [{Uuid, DevTok, Count}| Acc];
                           _    -> Acc
                       end end, [], IOS),
    lists:map(fun(Item) ->
                      lager:info("Removing token ~p ", [Item]),
                      dets:delete_object(IOS, Item) end, List),
    {noreply, State};

handle_cast({remove_android_token, RUuid, Token}, #state{ios = _IOS,
                                                  android = Android} = State) ->
    lager:info("Removing token ~p ", [{RUuid, Token}]),
    dets:delete_object(Android, {RUuid, Token}),
    {noreply, State};

handle_cast(All, State) ->
    lager:debug("unhandled handle_call ~p", [All]),
    {noreply, State}.

handle_info(Msg, State) ->
    lager:alert("Handle unknown info ~p", [Msg]),
    {noreply, State}.

terminate(Reason, State) ->
    lager:emergency("TERMINATING ~p~n~p", [Reason,erlang:get_stacktrace()]),
    db_w:close(State),
    ok.

code_change(OldVsn, State, "0.2e") ->
    lager:debug("UPGRADING VERSION ~n~p~n~p~n",[OldVsn, State]),
    {ok, State};

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
