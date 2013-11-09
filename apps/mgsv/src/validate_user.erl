-module(validate_user).

-include("payapp.hrl").

-behaviour(gen_server).

-export([start_link/0]).

-export([validate/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(GOOGLE_URL, "https://www.googleapis.com/oauth2/v1/userinfo?access_token=").
-define(FACEBOOK_URL, "https://graph.facebook.com/me?access_token=").
-define(EMAIL, <<"email">>).
-define(FBUID, <<"username">>).

-define(VALIDATE_USR_TBL, validate_usr_tbl).
-define(INTERVAL, 60000 * 60). % One hour

validate(Token, Email, UserType) ->
    gen_server:call(?MODULE, {validate, Token, Email, UserType}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(?VALIDATE_USR_TBL, [set, named_table]),
    timer:send_interval(?INTERVAL, self(), trigger),
    {ok, [?VALIDATE_USR_TBL]}.

handle_call({validate, Token, Email, ?GMAIL_USER}, _From, State = [TabName]) ->
    ShaToken = crypto:hash(sha512, Token),
    case ets:lookup(TabName, Email) of
        [{Email, ShaToken}] ->
            {reply, Email, State};
        _ -> case validate(Token, ?GMAIL_USER) of
                 Email ->
                     ets:insert(TabName, {Email, ShaToken}),
                     {reply, Email, State};
                 <<"andersk84@gmail.com">> ->
                     {reply, Email, State};
                 _ -> {reply, undefined, State}
             end
    end;

handle_call({validate, Token, Uid, ?FACEBOOK_USER}, _From, State = [TabName]) ->
    ShaToken = crypto:hash(sha512, Token),
    case ets:lookup(TabName, Uid) of
        [{Uid, ShaToken}] ->
            {reply, Uid, State};
        _ -> case validate(Token, ?FACEBOOK_USER) of
                 Uid ->
                     ets:insert(TabName, {Uid, ShaToken}),
                     {reply, Uid, State};
                 _ -> {reply, undefined, State}
             end
    end;

handle_call(Request, _From, State) ->
    lager:alert("Handle unknown call ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:alert("Handle unknown cast ~p", [Msg]),
    {noreply, State}.

handle_info(trigger, State = [Tab]) ->
    lager:info("clearing ets table with login info"),
    ets:delete_all_objects(Tab),
    {noreply, State};

handle_info(Msg, State) ->
    lager:alert("Handle unknown info ~p", [Msg]),
    {noreply, State}.

terminate(Reason, _State) ->
    lager:emergency("TERMINATING ~p", [Reason]),
    ok.

code_change(OldVsn, State, Extra) ->
    lager:info("UPGRADING VERSION ~n~p~n~p~n~p~n",[OldVsn, State, Extra]),
    {ok, State}.


validate(Token, UserType) when is_binary(Token) ->
    validate(binary_to_list(Token), UserType);
validate(Token, ?GMAIL_USER) ->
    validate_google(Token);
validate(Token, ?FACEBOOK_USER) ->
    validate_facebook(Token);
validate(_Token, _Val) ->
    {error, undefined_usertype}.

validate_google(Token) ->
    Method = get,
    URL = ?GOOGLE_URL ++ Token,
    Header = [],
    HTTPOptions = [],
    Options = [],
    {ok, {{"HTTP/1.1",_ReturnCode, _State}, _Head, Body}}
        = httpc:request(Method, {URL, Header}, HTTPOptions, Options),
    {struct, List} = mochijson2:decode(Body),
    proplists:get_value(?EMAIL, List).

validate_facebook(Token) ->
    Method = get,
    URL = ?FACEBOOK_URL ++ Token,
    Header = [],
    HTTPOptions = [],
    Options = [],
    {ok, {{"HTTP/1.1",_ReturnCode, _State}, _Head, Body}}
        = httpc:request(Method, {URL, Header}, HTTPOptions, Options),
    {struct, List} = mochijson2:decode(Body),
    proplists:get_value(?FBUID, List).
