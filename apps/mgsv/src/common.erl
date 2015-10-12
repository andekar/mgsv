-module(common).

-include("common.hrl").

-export([sb/1,mod_edit_details/2, get_timestamp/0, binary_uuid/0]).

%%string to binary
sb(Arg) when is_list(Arg) ->
    list_to_binary(Arg);
sb(Arg) ->
    Arg.

mod_edit_details(Edits = #edit_details{}, ReqBy = #user{}) ->
    Now = get_timestamp(),
    InternalUid = ReqBy#user.internal_uid,
    {CreatedBy, CreatedAt} =
        case Edits#edit_details.created_by of
            undefined ->
                {InternalUid, Now};
            By ->
                {By, Edits#edit_details.created_at}
        end,
    Edits#edit_details{
      created_at = CreatedAt,
      created_by = CreatedBy,
      last_change = Now,
      last_changed_by = InternalUid
     }.

get_timestamp() ->
    {Mega, Seconds, Milli} = erlang:timestamp(),
    Mega * 1000000000000 + Seconds * 1000000 + Milli.

binary_uuid() ->
    ossp_uuid:make(v4, text).
