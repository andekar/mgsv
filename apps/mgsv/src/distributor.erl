-module(distributor).

-export([content_types_provided/2, content_types_accepted/2,
         init/1, allowed_methods/2, from_json/2, to_html/2,
         is_authorized/2, delete_resource/2, delete_completed/2,
         process_post/2, resource_exists/2, malformed_request/2]).

-include_lib("webmachine/include/webmachine.hrl").

-include("payapp.hrl").

init(_Config) ->
    {ok, []}.

allowed_methods(ReqData, Context) ->
    {['GET', 'HEAD', 'PUT', 'DELETE', 'POST'], ReqData, Context}.

%%callbacks for the dispatch
content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}], ReqData, Context}.

content_types_accepted(RD, Ctx) ->
    {[{"application/json", from_json}, {"text/html", to_html}], RD, Ctx}.

resource_exists(ReqData, Context) ->
    case { ReqData#wm_reqdata.method
         , wrq:path_tokens(ReqData)
         , ReqData#wm_reqdata.scheme} of
        {'PUT', ["users"], https} ->
            {true, ReqData, Context};
        {'GET', ["users"|_More], https} ->
            {true, ReqData, Context};
        {'POST', ["users"|_More], https} ->
            {true, ReqData, Context};

        {'DELETE', ["debts", _Id], https} ->
            {true, ReqData, Context};
        {'PUT', ["debts"], https} ->
            {true, ReqData, Context};
        {'GET', ["debts"], https} ->
            {true, ReqData, Context};

        {'DELETE', ["transactions", _Id], https} ->
            {true, ReqData, Context};
        {'POST', ["transactions"], https} ->
            {true, ReqData, Context};
        {'GET', ["transactions"|_ToFrom], https} ->
            {true, ReqData, Context};
        {'GET', ["countries"], https} ->
            {true, ReqData, Context};
        {'GET', ["country", _CountryCode], https} ->
            {true, ReqData, Context};
        {'GET', ["rates"], https} ->
            {true, ReqData, Context};
        {'GET', ["rate", _CountryCode], https} ->
            {true, ReqData, Context};

        {'DELETE', ["feedback",_More], https} ->
            {true, ReqData, Context};
        {'POST', ["feedback"], https} ->
            {true, ReqData, Context};
        {'GET', ["feedback"], https} ->
            {true, ReqData, Context};

        {'POST', ["ios_token"], https} ->
            {true, ReqData, Context};
        {'DELETE', ["ios_token", _Badge], https} ->
            {true, ReqData, Context};

        {Method, Path, https} ->
            lager:alert("request denied method ~p path ~p", [Method, Path]),
            {false, ReqData, Context};
        _ -> {false, ReqData, Context} %% we do not want to change http version now
    end.

malformed_request(ReqData, Context) ->
    case {ReqData#wm_reqdata.method, ReqData#wm_reqdata.scheme} of
        {'POST', https} ->
            case request_data(wrq:req_body(ReqData)) of
                [] -> {true, ReqData, Context};
                Json ->
                    {false, ReqData, [{json, Json}| Context]}
            end;
        {'PUT', https} ->
            case request_data(wrq:req_body(ReqData)) of
                [] -> {true, ReqData, Context};
                Json ->
                    {false, ReqData, [{json, Json}| Context]}
            end;
        {'GET', https} ->
            {false, ReqData, Context};
        {'DELETE', https} ->
            {false, ReqData, Context};
        {_, http} ->
            {false, ReqData, Context}
    end.


process_post(ReqData, Context) ->
    Scheme = ReqData#wm_reqdata.scheme,
    Method = ReqData#wm_reqdata.method,
    Any = wrq:req_body(ReqData),
    Url = wrq:path_tokens(ReqData),
    [{json, Decoded}]  = proplists:lookup_all(json, Context),
    [{userdata, Auth}] = proplists:lookup_all(userdata, Context),
    Reply =
        case {replace_request_by(Decoded, Auth, Scheme), Scheme} of
            {{ok, Props}, https} -> error_logger:info_msg("~p ~p ~p ~p ~p", [Method, Url, Any, Props, Scheme]),
                                    try
                                        {ok, Result} = mgsv_server:send_message({Method, proplists:get_value(?REQUEST_BY, Props),
                                                                                 Url, Props, Scheme}),
                                        Result
                                    catch
                                        _:Error ->
                                            lager:alert("CRASH ~p", [Error]),
                                            mochijson2:encode([[{error,request_failed}]])
                                    end;
            _ -> mochijson2:encode([[{error, user_not_authenticated}]])
        end,
    HBody = io_lib:format("~s~n", [erlang:iolist_to_binary(Reply)]),
    error_logger:info_msg("REPLY ~s",[HBody]),
    {true, wrq:set_resp_header("Content-type", "application/json", wrq:append_to_response_body(HBody, ReqData)), Context}.

delete_resource(ReqData, Context) ->
    Url = wrq:path_tokens(ReqData),
    Method = ReqData#wm_reqdata.method,
    Scheme = ReqData#wm_reqdata.scheme,

    lager:info("~p ~p", [Method, Url]),
    {_UserType, UserId, _Token} = user_from_auth(wrq:get_req_header("authorization", ReqData)),
    case catch mgsv_server:send_message({Method, list_to_binary(UserId),
                                         Url, Scheme}) of
        ok -> {true, ReqData, Context};
        {'EXIT', Error} ->
            lager:alert("CRASH ~p", [Error]),
            {false, ReqData, Context};
        _  -> {false, ReqData, Context}
    end.

delete_completed(ReqData, Context) ->
    lager:info("Delete completed"),
    {true, ReqData, Context}.

from_json(ReqData, Context) ->
    Scheme = ReqData#wm_reqdata.scheme,
    Method = ReqData#wm_reqdata.method,
    Url = wrq:path_tokens(ReqData),
    [{json, Decoded}] = proplists:lookup_all(json, Context),
    [{userdata, Auth}] = proplists:lookup_all(userdata, Context),
    Reply =
        case {replace_request_by(Decoded, Auth, Scheme), Scheme} of
            {{ok, _Props}, http} ->
                "operation not allowed";

            {{ok, Props}, https} -> error_logger:info_msg("~p ~p ~p", [Method, Url, Props]),
                                    try
                                        {ok, Result} = mgsv_server:send_message({Method, proplists:get_value(?REQUEST_BY, Props),
                                                                                 Url, Props, Scheme}),
                                        Result
                                    catch
                                        _:Error ->
                                            lager:alert("CRASH ~p", [Error]),
                                            mochijson2:encode([[{error,request_failed}]])
                                    end;
            _ -> mochijson2:encode([[{error, user_not_authenticated}]])
        end,
    error_logger:info_msg("REPLY ~s",[erlang:iolist_to_binary(Reply)]),
    HBody = io_lib:format("~s~n", [erlang:iolist_to_binary(Reply)]),
    {HBody, wrq:set_resp_header("Content-type", "application/json", wrq:append_to_response_body(Reply, ReqData)), Context}.


to_html(ReqData, Context) ->
    Scheme = ReqData#wm_reqdata.scheme,
    Method = ReqData#wm_reqdata.method,
    {Body, _RD, Ctx2} =
        case {wrq:path_tokens(ReqData), Scheme} of
            {_Any, http} ->
                Result = "Operation not allowed",
                {Result, ReqData, Context};
            {Any, https} ->
                error_logger:info_msg("~p ~p",[Method, Any]),
                {_UserType, UserId, _Token} = user_from_auth(wrq:get_req_header("authorization", ReqData)),
                {ok, Result} = try mgsv_server:send_message({Method, list_to_binary(UserId),
                                                             Any, Scheme})
                               catch
                                   _:Err ->
                                       lager:alert("CRASH ~p", [Err]),
                                       {ok, mochijson2:encode([[{error,request_failed}]])}
                               end,
                {Result, ReqData, Context}
        end,
    error_logger:info_msg("REPLY ~s",[erlang:iolist_to_binary(Body)]),

    HBody = io_lib:format("~s~n", [erlang:iolist_to_binary(Body)]),
    {HBody, ReqData, Ctx2}.

is_authorized(ReqData, Context) ->
    Scheme = ReqData#wm_reqdata.scheme,
    case Scheme of
        https ->
            case user_from_auth(wrq:get_req_header("authorization", ReqData)) of
                {"debug", _UserId, _Token} = UserData ->
                    {true, ReqData, [{userdata, UserData}|Context]};
                {UserType, UserId, Token} = UserData ->
                    BinUserId = list_to_binary(UserId),
                    lager:info("Request from ~p",[UserId]),
                    case validate_user:validate(list_to_binary(Token), BinUserId, list_to_binary(UserType)) of
                        <<"andersk84@gmail.com">> ->
                            {true, ReqData, [{userdata, UserData}|Context]};
                        BinUserId ->
                            {true, ReqData, [{userdata, UserData}|Context]};
                        Res ->
                            lager:alert("Access denied authorization field, usertype ~p userid ~p  res ~p", [UserType, UserId, Res]),
                            {"Basic realm=webmachine", ReqData, Context}
                    end;
                Any ->
                    lager:alert("Access denied authorization field: ~p", [Any]),
                    {"Basic realm=webmachine", ReqData, Context}
            end;
        _ -> {false, ReqData, Context}
    end.

destructify(List) when is_list(List)->
    lists:map(fun destructify/1, List);
destructify({?JSONSTRUCT, Val}) ->
    destructify(Val);
destructify({Key, PossibleList}) ->
    {Key, destructify(PossibleList)};
destructify(Other) ->
    Other.

replace_request_by(Props, _Any, http) ->
    {ok, Props};
replace_request_by(Props, {_UserType, UserId, _Token}, https) ->
    {ok, replace_prop(?REQUEST_BY, Props, list_to_binary(UserId))};
replace_request_by(_,_,_) ->
    {error,failed}.


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

replace_prop(Key, List, Value) ->
    [{Key, Value} | proplists:delete(Key, List)].


request_data(Data) ->
    try lists:flatten(destructify(mochijson2:decode(Data)))
    catch
        _:Errors ->
            lager:alert("CRASH when decoding json ~p", [Errors]),
            []
    end.
