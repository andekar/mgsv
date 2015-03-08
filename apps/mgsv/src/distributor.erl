-module(distributor).

-export([content_types_provided/2, content_types_accepted/2,
         init/1, allowed_methods/2, from_json/2, to_html/2,
         delete_resource/2, delete_completed/2,
         process_post/2, resource_exists/2, malformed_request/2,
         service_available/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

-include("payapp.hrl").
-include("common.hrl").
init(_Config) ->
    {ok, []}.

allowed_methods(ReqData, Context) ->
    {['GET', 'HEAD', 'PUT', 'DELETE', 'POST'], ReqData, Context}.

%%callbacks for the dispatch
content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}], ReqData, Context}.

content_types_accepted(RD, Ctx) ->
    {[{"application/json", from_json}, {"text/html", to_html}], RD, Ctx}.

%% service only available for https
service_available(ReqData, Context) ->
    case ReqData#wm_reqdata.scheme of
        https ->
            {true, ReqData, Context};
        _ ->
            {false, ReqData, Context}
    end.

resource_exists(ReqData, Context) ->
    case { ReqData#wm_reqdata.method
         , wrq:path_tokens(ReqData)} of
        {'PUT', ["users"]} ->
            {true, ReqData, Context};
        {'GET', ["users"|_More]} ->
            {true, ReqData, Context};
        {'POST', ["users"|_More]} ->
            {true, ReqData, Context};

        {'DELETE', ["debts", _Id]} ->
            {true, ReqData, Context};
        {'PUT', ["debts"]} ->
            {true, ReqData, Context};
        {'GET', ["debts"]} ->
            {true, ReqData, Context};

        {'DELETE', ["transactions", _Id]} ->
            {true, ReqData, Context};
        {'POST', ["transactions"]} ->
            {true, ReqData, Context};
        {'GET', ["transactions"|_ToFrom]} ->
            {true, ReqData, Context};
        {'GET', ["countries"]} ->
            {true, ReqData, Context};
        {'GET', ["country", _CountryCode]} ->
            {true, ReqData, Context};
        {'GET', ["rates"]} ->
            {true, ReqData, Context};
        {'GET', ["rate", _CountryCode]} ->
            {true, ReqData, Context};

        {'POST', ["ios_token"]} ->
            {true, ReqData, Context};
        {'POST', ["android_token"]} ->
            {true, ReqData, Context};
        {'DELETE', ["android_token", _Badge]} ->
            {true, ReqData, Context};
        {'DELETE', ["ios_token", _Badge]} ->
            {true, ReqData, Context};

        {Method, Path} ->
            lager:alert("request denied method ~p path ~p", [Method, Path]),
            {false, ReqData, Context}
    end.

%% at this stage there has been no authorization
%% but we still have the "to be authorized" data
malformed_request(OReqData, Context) ->

    %% we need to be authorized a bit earlier than what is usual..
    {Authorized, ReqData, NewCtx} = is_authorized(OReqData, Context),

    [{userdata, UD}] = proplists:lookup_all(userdata, NewCtx),
    lager:info("logged in as ~p", [UD]),
    Path = wrq:path_tokens(ReqData),
    lager:info("Request at path ~p", [Path]),
    Method = ReqData#wm_reqdata.method,
    case {Authorized, UD} of
        {true, #user_data{ protocol = undefined}} ->
            {true, ReqData, Context};
        {true, #user_data{}} when Method == 'POST' orelse
                                  Method == 'PUT' ->
            case request_data(Method, Path, wrq:req_body(ReqData), UD) of
                [] ->
                    {true, ReqData, Context};
                Json ->
                    lager:info("request ~p~n~n", [Json]),
                    {false, ReqData, [{json, Json}| NewCtx]}
            end;
        {true, #user_data{}} ->
            {false, ReqData, NewCtx};
        {true,_} ->
            {true, ReqData, Context};
        _ ->
            {{halt, 401}, ReqData, Context}

    end.

validate_req('POST', ["users"|_], Data, UD) ->
    case catch lists:map(fun(V) ->
                                 users:from_proplist(V, UD) end,
                                Data) of
        [#user{}|_] ->
            Data;
        E ->
            lager:error("Failed to unpack data ~p", [E]),
            []
    end;
validate_req('PUT', ["users"], Data, UD) ->
    case UD#user_data.user of
        U = #user{} ->
            case catch users:update_parts(Data, U, UD) of
                #user{} ->
                    Data;
                E ->
                    lager:error("failed to change ~p", [E]),
                    []
            end;
        _ ->
            lager:error("failed to change"),
            []
    end;
validate_req('POST', ["transactions"], Data, UD) ->
    case catch lists:map(fun(T) ->
                                 transaction:from_proplist(T, UD) end,
                         Data) of
        [#transaction{}|_] ->
            Data;
        E ->
            lager:error("failed to change ~p", [E]),
            []
    end;
validate_req('POST', ["ios_token"], Data, _UD) ->
    case Data of
       [{?IOS_TOKEN, _Token}] ->
            Data;
        E  ->
            lager:error("failed to change ~p", [E]),
            []
    end;
validate_req('POST', ["android_token"], Data, _UD) ->
    case Data of
       [{?ANDROID_TOKEN, _Token}] ->
            Data;
        E  ->
            lager:error("failed to change ~p", [E]),
            []
    end;
validate_req('PUT', ["debts"], Data, _UD) ->
    case {proplists:lookup_all(?OLD_UID, Data),
          proplists:lookup_all(?NEW_UID, Data)} of
        {[{?OLD_UID,_}],[{?NEW_UID,_}]} ->
            Data;
        E ->
            lager:error("failed to change ~p", [E]),
            []
    end;

validate_req(_Method, _Path, Data, _UD) ->
    lager:info("returning data ~p~n",[Data]),
    Data.


process_post(ReqData, Context) ->
    Method = ReqData#wm_reqdata.method,
    Any = wrq:req_body(ReqData),
    Url = wrq:path_tokens(ReqData),
    [{json, Decoded}]  = proplists:lookup_all(json, Context),
    [{userdata, UD}] = proplists:lookup_all(userdata, Context),
    error_logger:info_msg("~p ~p ~p ~p", [Method, Url, Any, UD]),
    Reply = try
                {ok, Result} = mgsv_server:send_message({Method, UD,
                                                         Url, Decoded}),
                Result
            catch
                _:Error ->
                    lager:alert("CRASH ~p~n~p~n", [Error, erlang:get_stacktrace()]),
                    mochijson2:encode([[{error,request_failed}]])
            end,

    HBody = io_lib:format("~s~n", [erlang:iolist_to_binary(Reply)]),
    error_logger:info_msg("REPLY ~s",[HBody]),
    {true, wrq:set_resp_header("Content-type", "application/json", wrq:append_to_response_body(HBody, ReqData)), Context}.

delete_resource(ReqData, Context) ->
    Url = wrq:path_tokens(ReqData),
    Method = ReqData#wm_reqdata.method,

    lager:info("~p ~p", [Method, Url]),
    [{userdata, UD}] = proplists:lookup_all(userdata, Context),
    case catch mgsv_server:send_message({Method, UD,
                                         Url}) of
        ok -> {true, ReqData, Context};
        {'EXIT', Error} ->
            lager:alert("CRASH ~p~n~p", [Error, erlang:get_stacktrace()]),
            {false, ReqData, Context};
        _  -> {false, ReqData, Context}
    end.

delete_completed(ReqData, Context) ->
    lager:info("Delete completed"),
    {true, ReqData, Context}.

from_json(ReqData, Context) ->
    Method = ReqData#wm_reqdata.method,
    Url = wrq:path_tokens(ReqData),
    [{json, Decoded}] = proplists:lookup_all(json, Context),
    [{userdata, Ud}] = proplists:lookup_all(userdata, Context),
    SReply = try
                error_logger:info_msg("~p ~p ~p", [Method, Url, Decoded]), %
                mgsv_server:send_message({Method, Ud,
                                          Url, Decoded})
            catch
                _:TheError ->
                    lager:alert("CRASH ~p~n~p", [TheError, erlang:get_stacktrace()]),
                    mochijson2:encode([[{error,request_failed}]])
            end,
    case SReply of
        {ok, Reply} ->
            error_logger:info_msg("REPLY ~s",[erlang:iolist_to_binary(Reply)]),
            HBody = io_lib:format("~s~n", [erlang:iolist_to_binary(Reply)]),
            {HBody, wrq:set_resp_header("Content-type", "application/json", wrq:append_to_response_body(Reply, ReqData)), Context};
        {nok, Error} ->
            error_logger:info_msg("REPLY 400 error ~p",[Error]),
            {{halt, 400}, ReqData, Context}
    end.

to_html(ReqData, Context) ->
    Method = ReqData#wm_reqdata.method,
    Any = wrq:path_tokens(ReqData),
    error_logger:info_msg("~p ~p",[Method, Any]),
    [{userdata, UD}] = proplists:lookup_all(userdata, Context),
    {ok, Body} = try mgsv_server:send_message({Method, UD,
                                               Any})
                 catch
                     _:Err ->
                         lager:alert("CRASH ~p~n~p", [Err, erlang:get_stacktrace()]),
                         {ok, mochijson2:encode([[{error,request_failed}]])}
                 end,
    case wrq:path_tokens(ReqData) of
        ["countries"] ->
            error_logger:info_msg("REPLY {\"NZD\":\"New Zealand Dollar....");
        ["rates"] ->
            error_logger:info_msg("REPLY {\"UGX\ ...");
        _ -> error_logger:info_msg("REPLY ~s",[erlang:iolist_to_binary(Body)])
    end,

    HBody = io_lib:format("~s~n", [erlang:iolist_to_binary(Body)]),
    {HBody, ReqData, Context}.

is_authorized(ReqData, Context) ->
    UserAgent = string:tokens(wrq:get_req_header("user-agent", ReqData), "/ "),
    ExpectedProtocol = wrq:get_req_header("protocolversion", ReqData),
    %%PayApp/1.4 CFNetwork/709.1 Darwin/13.3.0"
    {_,{Os,Version}} = lists:foldl(fun("PayApp",{false,Vars}) ->
                                           {true,Vars};
                                      (V,{true,{O,_}}) ->
                                           {false,{O,V}};
                                      ("Darwin", {B,{_,V}})->
                                           {B,{ios,V}};
                                      (_, {false,Vars}) ->
                                           {false,Vars}
                                   end,
                                   {false,{android,"0.0"}}, UserAgent), %% Find out ios version etc for now
    lager:info("Got user-agent OS ~p Version ~p Protocol ~p~n~p~n",
               [Os,Version,ExpectedProtocol, UserAgent]),
    UD = case user_from_auth(wrq:get_req_header("authorization", ReqData)) of
             {UserType, UserId, Token} ->
                 BinUId = list_to_binary(UserId),
                 BinToken = list_to_binary(Token),
                 #user_data{os = Os,
                            version = Version,
                            protocol = ExpectedProtocol,
                            username = BinUId,
                            id = BinToken,
                            user_type = list_to_binary(UserType),
                            user = users:get(BinUId) % maybe a bit too early
                           };
             _ ->
                 malformed_error
         end,


    IsTestServer = case application:get_env(mgsv, test_server) of
                       {ok, Bool} ->
                           Bool;
                       _ ->
                           false
                   end,
    ECtx = proplists:delete(userdata, Context),
    BinUserId = UD#user_data.username,
    case UD#user_data.user_type of
        <<"debug">> when IsTestServer ->
            {true, ReqData, [{userdata, UD}|ECtx]};
        UT -> %% note we have hidden the key token in user_data.id
            case validate_user:validate(UD#user_data.id, BinUserId, UT) of
                {BinUserId, Id} ->
                    {true, ReqData, [{userdata, UD#user_data{id = Id}}|
                                     ECtx]};
                {BinUserName, BinUserId} ->
                            {true, ReqData, [{userdata, UD#user_data{id = BinUserId,
                                                                     username = BinUserName}}|
                                             ECtx]};
                Res ->
                    lager:alert("Access denied authorization field, userdata ~p res ~p", [UD#user_data{id = hidden}, Res]),
                    {"Basic realm=webmachine", ReqData, Context}
            end
    end.

destructify(List) when is_list(List)->
    lists:map(fun destructify/1, List);
destructify({?JSONSTRUCT, Val}) ->
    destructify(Val);
destructify({Key, PossibleList}) ->
    {Key, destructify(PossibleList)};
destructify(Other) ->
    Other.

user_from_auth("Basic" ++ Base64) ->
    Str = base64:mime_decode_to_string(Base64),
    case string:tokens(Str, ":") of
        [UserType, UserId, Token] ->
            {UserType, UserId, Token};
        Any ->
            lager:error("trying to decode authorization field, got ~p", [Any]),
            {error, wrong_format}
    end;
user_from_auth(_) ->
    {error, wrong_format}.

request_data(Method, Path, Data, UD) ->
    try T = lists:flatten(destructify(mochijson2:decode(Data))),
          lager:info("Got data ~p~n~n", [T]),
          validate_req(Method,Path,T, UD)
    catch
        _:Errors ->
            lager:alert("CRASH when decoding json ~p~n~p~nData: ~p", [Errors, erlang:get_stacktrace()
, Data]),
            []
    end.
