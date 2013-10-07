-module(distributor).

-export([content_types_provided/2, content_types_accepted/2,
         init/1, allowed_methods/2, from_json/2, to_html/2,
         is_authorized/2, delete_resource/2, delete_completed/2,
         process_post/2]).

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

process_post(ReqData, Context) ->
    lager:info("process_post"),
    Scheme = ReqData#wm_reqdata.scheme,
    Method = ReqData#wm_reqdata.method,
    Any = wrq:req_body(ReqData),
    Url = wrq:path_tokens(ReqData),
    Decoded = lists:flatten(destructify(mochijson2:decode(Any))),
    Reply =
        case {validate_replace_request_by(Decoded, Scheme), Scheme} of
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
    error_logger:info_msg("REPLY ~s",[erlang:iolist_to_binary(Reply)]),
    HBody = io_lib:format("~s~n", [erlang:iolist_to_binary(Reply)]),
    {true, wrq:set_resp_header("Content-type", "application/json", wrq:append_to_response_body(HBody, ReqData)), Context}.

delete_resource(ReqData, Context) ->
    Url = wrq:path_tokens(ReqData),
    Method = ReqData#wm_reqdata.method,
    Scheme = ReqData#wm_reqdata.scheme,

    lager:info("~p ~p", [Method, Url]),
    case mgsv_server:send_message({Method, <<"andersk84@gmail.com">>,
                                   Url, Scheme}) of
        ok -> {true, ReqData, Context};
        _  -> {false, ReqData, Context}
    end.

delete_completed(ReqData, Context) ->
    lager:info("Delete completed"),
    {true, ReqData, Context}.

from_json(ReqData, Context) ->
    Scheme = ReqData#wm_reqdata.scheme,
    Method = ReqData#wm_reqdata.method,
    Any = wrq:req_body(ReqData),
    Url = wrq:path_tokens(ReqData),
    Decoded = lists:flatten(destructify(mochijson2:decode(Any))),

    Reply =
        case {validate_replace_request_by(Decoded, Scheme), Scheme} of
            {{ok, Props}, http} -> error_logger:info_msg("~p ~p ~p", [Method, Url, Props]),
                                   try
                                       {ok, Result} = mgsv_server:send_message({Url, Props, Scheme}),
                                       Result
                                   catch
                                       _:Error ->
                                           lager:alert("CRASH ~p", [Error]),
                                           mochijson2:encode([[{error,request_failed}]])
                                   end;
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
            {Any, http} ->
                error_logger:info_msg("~p ~p",[Method, Any]),
                {ok, Result} = try mgsv_server:send_message({Any, Scheme})
                               catch
                                   _:Err ->
                                       lager:alert("CRASH ~p", [Err]),
                                       {ok, mochijson2:encode([[{error,request_failed}]])}
                               end,
                {Result, ReqData, Context};
            {Any, https} ->
                error_logger:info_msg("~p ~p",[Method, Any]),
                {ok, Result} = try mgsv_server:send_message({Method, <<"andersk84@gmail.com">>,
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
            case wrq:get_req_header("authorization", ReqData) of
                "Basic" ++ Base64 ->
                    Str = base64:mime_decode_to_string(Base64),
                    case string:tokens(Str, ":") of
                        [UserId, Token] ->
                            lager:info("Request from ~p",[UserId]),
                            case validate_user:validate(Token, ?GMAIL_USER) of
                                "andersk84@gmail.com" -> 
                                    {true, ReqData, Context};
                                UserId ->
                                    {true, ReqData, Context};
                                _ ->
                                    {false, ReqData, Context}
                            end;

                        Any ->
                            lager:alert("Access denied authorization field: ~p", [Any]),
                            {"Basic realm=webmachine", ReqData, Context}
                    end;
                _ ->
                    lager:alert("Access denied due to no authorization string in header request"),
                    {"Basic realm = ", ReqData, Context}
            end;
        _ -> {true, ReqData, Context}
    end.

destructify(List) when is_list(List)->
    lists:map(fun destructify/1, List);
destructify({?JSONSTRUCT, Val}) ->
    destructify(Val);
destructify({Key, PossibleList}) ->
    {Key, destructify(PossibleList)};
destructify(Other) ->
    Other.

validate_replace_request_by(Props, http) ->
    {ok, Props};

validate_replace_request_by(Props, https) ->
    case proplists:get_value(?REQUEST_BY, Props) of
        undefined -> [{error, missing_request_by}];
        Val -> case validate_user:validate(Val, ?GMAIL_USER) of
                   undefined ->
                       [{error, invalid_user}];
                   "andersk84@gmail.com" ->
                       case proplists:get_value(?DEBUG_AS, Props) of
                           undefined ->{ok, replace_prop(?REQUEST_BY, Props, "andersk84@gmail.com")};
                           DebugUser -> {ok, replace_prop(?REQUEST_BY, Props, DebugUser)}
                       end;
                   User -> {ok, replace_prop(?REQUEST_BY, Props, User)}
               end
    end.


replace_prop(Key, List, Value) ->
    [{Key, Value} | proplists:delete(Key, List)].
