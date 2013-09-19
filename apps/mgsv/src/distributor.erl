-module(distributor).

-export([content_types_provided/2, content_types_accepted/2,
         init/1, allowed_methods/2, from_json/2, to_html/2]).

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
    try
        Reply = case validate_replace_request_by(Decoded, Scheme) of
                    {ok, Props} -> error_logger:info_msg("PUT ~p", [Props]),
                                   {ok, Result} = mgsv_server:send_message({Url, Props, Scheme}),
                                   Result;
                    _ -> mochijson2:encode([{error, user_not_authenticated}])
                end,
        error_logger:info_msg("REPLY ~s",[erlang:iolist_to_binary(Reply)]),
        HBody = io_lib:format("~s~n", [erlang:iolist_to_binary(Reply)]),
        {HBody, wrq:set_resp_header("Content-type", "application/json", wrq:append_to_response_body(Reply, ReqData)), Context}
    catch
        Error ->
            lager:alert("CRASH ~p", [Error]),
            Replied = [{error,request_failed}],
            error_logger:info_msg("REPLY ~s",[erlang:iolist_to_binary(Replied)]),
            HBody2 = io_lib:format("~s~n", [erlang:iolist_to_binary(Replied)]),
            {HBody2, wrq:set_resp_header("Content-type", "application/json", wrq:append_to_response_body(Replied, ReqData)), Context}
    end.


to_html(ReqData, Context) ->
    Scheme = ReqData#wm_reqdata.scheme,
    try
        {Body, _RD, Ctx2} = case wrq:path_tokens(ReqData) of
                                Any ->
                                    error_logger:info_msg("GET ~p",[Any]),
                                    {ok, Result} = mgsv_server:send_message({Any, Scheme}),
                                    {Result, ReqData, Context}
                            end,
        error_logger:info_msg("REPLY ~s",[erlang:iolist_to_binary(Body)]),

        HBody = io_lib:format("~s~n", [erlang:iolist_to_binary(Body)]),
        {HBody, ReqData, Ctx2}
    catch
        Error ->
            lager:alert("CRASH ~p", [Error]),
            Replied = [{error,request_failed}],
            error_logger:info_msg("REPLY ~s",[erlang:iolist_to_binary(Replied)]),
            HBody2 = io_lib:format("~s~n", [erlang:iolist_to_binary(Replied)]),
            {HBody2, wrq:set_resp_header("Content-type", "application/json", wrq:append_to_response_body(Replied, ReqData)), Context}
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
                   User -> {ok, replace_prop(?REQUEST_BY, Props, User)}
               end
    end.


replace_prop(Key, List, Value) ->
    [{Key, Value} | proplists:delete(Key, List)].
