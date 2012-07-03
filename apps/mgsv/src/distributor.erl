-module(distributor).

-export([content_types_provided/2, content_types_accepted/2,
         init/1, allowed_methods/2, from_json/2, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
    {ok, []}.

allowed_methods(ReqData, Context) ->
    {['GET', 'HEAD', 'PUT'], ReqData, Context}.

%%callbacks for the dispatch
content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}], ReqData, Context}.

content_types_accepted(RD, Ctx) ->
    error_logger:info_msg("Content types~n"),
    {[{"application/json", from_json}, {"text/html", to_html}], RD, Ctx}.

from_json(ReqData, Context) ->
    Tmp = case wrq:req_body(ReqData) of
         Any ->
                    error_logger:info_msg("JSon received ~p~n",[Any]),
                    Decoded = mochijson2:decode(Any),
                    {ok, Result} = mgsv_server:send_message(Decoded),
                    {Result, ReqData, Context};
         _ ->       Body2 = io_lib:format("OK", []),
                    {Body2, ReqData, Context}
                    end,
    {"OK", ReqData, Context}.

to_html(ReqData, Context) ->
    error_logger:info_msg("to_html~n",[]),
    {Body, _RD, Ctx2} = case wrq:path_tokens(ReqData) of
         Any ->
                    error_logger:info_msg("Get request received~n~p~n",[Any]),
                    {ok, Result} = mgsv_server:send_message(Any),
                    {Result, ReqData, Context};
         _ ->       Body2 = io_lib:format("OK", []),
                    {Body2, ReqData, Context}
                    end,
    HBody = io_lib:format("~s~n", [erlang:iolist_to_binary(Body)]),
    {HBody, ReqData, Ctx2}.