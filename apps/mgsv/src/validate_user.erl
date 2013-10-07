-module(validate_user).

-include("payapp.hrl").

-export([validate/2]).

-define(GOOGLE_URL, "https://www.googleapis.com/oauth2/v1/userinfo?access_token=").
-define(EMAIL, <<"email">>).

validate(Token, ?GMAIL_USER) ->
    validate_google(binary_to_list(Token));
validate(_Token, _) ->
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

