-module(distributor).

-export([content_types_provided/2, content_types_accepted/2,
         init/1, allowed_methods/2, from_json/2, to_html/2, is_authorized/2]).

-include_lib("webmachine/include/webmachine.hrl").

-include("payapp.hrl").

init(_Config) ->
    {ok, []}.

allowed_methods(ReqData, Context) ->
    {['GET', 'HEAD', 'PUT'], ReqData, Context}.

%%callbacks for the dispatch
content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}], ReqData, Context}.

content_types_accepted(RD, Ctx) ->
    {[{"application/json", from_json}, {"text/html", to_html}], RD, Ctx}.

from_json(ReqData, Context) ->
    Scheme = ReqData#wm_reqdata.scheme,
    Any = wrq:req_body(ReqData),
    Url = wrq:path_tokens(ReqData),
    Decoded = lists:flatten(destructify(mochijson2:decode(Any))),

    Reply =
        case validate_replace_request_by(Decoded, Scheme) of
            {ok, Props} -> error_logger:info_msg("PUT ~p ~p", [Url, Props]),
                           try
                               {ok, Result} = mgsv_server:send_message({Url, Props, Scheme}),
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
    {Body, _RD, Ctx2} =
        case wrq:path_tokens(ReqData) of
            Any ->
                error_logger:info_msg("GET ~p",[Any]),
                {ok, Result} = try mgsv_server:send_message({Any, Scheme})
                               catch
                                   _:Err ->
                                       lager:alert("CRASH ~p", [Err]),
                                       mochijson2:encode([[{error,request_failed}]])
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
                            {true, ReqData, Context};
                        _ ->
                            {"Basic realm=webmachine", ReqData, Context}
                    end;
                _ ->
                    lager:alert("Access denied due to no authorization string in header request"),
                    {"Basic realm = ", ReqData, Context}
            end;
        _ -> {true, ReqData, Context}
    end.

%    Req = ?REQ(ReqProps),
%    case Req:get_header_value("authorization") of
%        "Basic "++Base64 ->
%            Str = base64:mime_decode_to_string(Base64),
%            case string:tokens(Str, ":") of
%                ["authdemo", "demo1"] ->
%                    {true, Context};
%                _ ->
%                    {"Basic realm=webmachine", Context}
%            end;
%        _ ->
%            {"Basic realm=webmachine", Context}
%    end;
%    {true, Context}.

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
