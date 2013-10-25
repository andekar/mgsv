-module(exchange_rate).

-include("payapp.hrl").


-export([rate/2]).

-define(GOOGLE_URL, "http://www.google.com/ig/calculator?hl=en&q=1").


rate(From, To) when is_binary(From) ->
    rate(binary_to_list(From), To);
rate(From, To) when is_binary(To) ->
    rate(From, binary_to_list(To));
rate(From, To) ->
    Method = get,
    URL = ?GOOGLE_URL ++ From ++ "=?" ++ To,
    Header = [],
    HTTPOptions = [],
    Options = [],
    {ok, {{"HTTP/1.1",_ReturnCode, _State}, _Head, Body}}
        = httpc:request(Method, {URL, Header}, HTTPOptions, Options),
    [_|StrVal] = lists:nth(5,string:tokens(Body, " ")),
    {Value, []} = string:to_float(StrVal),
    Value.
