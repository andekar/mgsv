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
    Any = wrq:req_body(ReqData),
    Url = wrq:path_tokens(ReqData),
    error_logger:info_msg("JSon received ~p at url ~p",[Any, Url]),
    Decoded = lists:flatten(destructify_list(mochijson2:decode(Any))),
    error_logger:info_msg("Decoded to ~p", [Decoded]),
    {ok, Result} = mgsv_server:send_message({Url, Decoded}),

    HBody = io_lib:format("~s~n", [erlang:iolist_to_binary(Result)]),
    {HBody, wrq:set_resp_header("Content-type", "text/html", wrq:append_to_response_body(Result, ReqData)), Context}.

to_html(ReqData, Context) ->
    {Body, _RD, Ctx2} = case wrq:path_tokens(ReqData) of
         Any ->
                    error_logger:info_msg("Get request received ~p",[Any]),
                    {ok, Result} = mgsv_server:send_message(Any),
                    {Result, ReqData, Context}
        end,
    HBody = io_lib:format("~s~n", [erlang:iolist_to_binary(Body)]),
    {HBody, ReqData, Ctx2}.

destructify_list(List) when is_list(List)->
    lists:map(fun destructify/1, List);
destructify_list(Other) ->
    Other.

destructify({?JSONSTRUCT, Val}) ->
    destructify_list(Val);
destructify(Any) ->
    Any.


