-module(users).

-include("common.hrl").
-include("payapp.hrl").

-export([create/6,
         add/6,
         add/2,
         create_mappingtable/0,
         create_usertable/0,
         get/1,
         delete/1,
         update/2,
         reconstruct/1,
         update_parts/2,
         to_proplist/2,
         count_by_usertype/1,
         update_find/1,
         from_proplist/2
        ]).

-record(user_mapping,
        { uid :: binary(), %% id from google or facebook or internal if local
          username :: binary(), %% username from google or facebook or internal if local
          internal_uid :: binary(), %% unique id for server
          user_type :: binary(), %% I think string
          user_mapping_details  = #edit_details{} :: #edit_details{}
        }).

-record(user_info,
        { internal_uid = common:binary_uuid():: binary(), %% unique id for server
          displayname :: binary(), %% name to display
          currency :: binary(),
          user_edit_details = #edit_details{} :: #edit_details{}
        }).

add(_Uid, _Username, DisplayName, ?LOCAL_USER, Currency, ReqBy = #user{}) ->
    Username = common:binary_uuid(),
    User = create(Username, Username, DisplayName, ?LOCAL_USER, Currency, ReqBy),
    add(User, ReqBy),
    User;
add(Uid, Username, DisplayName, UserType, Currency, ReqBy = #user{}) ->
    User = create(Uid, Username, DisplayName, UserType, Currency, ReqBy),
    add(User, ReqBy),
    User;
add(Uid, Username, DisplayName, UserType, Currency, Uid) ->
    User = create(Uid, Username, DisplayName, UserType, Currency, Uid),
    add(User, User),
    User.

add(User, #user_data{ user = no_such_user, %% adding self
                      username = Username,
                      id = Id
                    }) ->
    case User#user.username of
        Username ->
            ReqBU = #user{ uid = Id,
                           username = Username
                         },
            add(User#user{ uid = Id }, ReqBU);
        _ ->
            illegal_aciton %% trying to add some one else
    end;
add(User, #user_data{ user = ReqBU}) ->
    add(User, ReqBU);
add(User, ReqBy = #user{}) ->
    case [users:get({username, User#user.username}),
          users:get({uid, User#user.uid})] of
        [no_such_user, no_such_user] ->
            UserMapping = create_mapping(User, ReqBy),
            UserInfo = create_userinfo(User,ReqBy),
            mnesia:dirty_write(UserMapping),
            mnesia:dirty_write(UserInfo);
        [UUser = #user{},_] ->
            UUser;
        [_, UUser = #user{}] ->
            UUser
    end.

%%Todo what if not registered?!?
-spec update_find(#user_data{}) -> #user{} | no_such_user.
update_find(Userdata = #user_data{}) ->
    Id = Userdata#user_data.id,
    Username = Userdata#user_data.username,
    U = case users:get({uid, Id}) of
            #user{ username = Username} = U1 ->  %% all is well here
                U1;
            #user{} = U2 ->
                lager:error("Something is wrong, user might have changed username ~p~n~p~n", [U2, Userdata]),
                no_such_user;
            no_such_user ->
                lager:info("users:get({uid, ~p})~nTrying request with invalid user ~p", [Id, Userdata]),
                case users:get({username, Username}) of
                    #user{ username = Username,
                           uid = Username} = U3 -> %% here we must update uid also we should check if this user is already existing.....

                        UUser = U3#user{uid = Id},
                        update_remove_usermapping(U3, UUser),
                        lager:info("Updated user from ~n~p~nto~n~p~n", [U3, UUser]),
                        UUser;
                    _ ->
                        lager:info("users:get({username, ~p})~nTrying request with invalid user ~p", [Username, Userdata]),
                        no_such_user
                end
        end,
    Userdata#user_data{user = U}.

update(User, ReqBy) ->
    UserInfo = create_userinfo(User,ReqBy),
    mnesia:dirty_write(UserInfo).

update_remove_usermapping(OldUser,NewUser) ->
    mnesia:dirty_delete(user_mapping, OldUser#user.uid),
    UserMapping = create_mapping(NewUser, NewUser),
    mnesia:dirty_write(UserMapping).

delete(User = #user{}) ->
    mnesia:dirty_delete(user_info, User#user.internal_uid),
    mnesia:dirty_delete(user_mapping, User#user.uid);
delete(Uid) ->
    delete(users:get(Uid)).

create(Uid, Username, DisplayName, UserType, Currency, ReqBy = #user{}) ->
    #user{
       uid = common:sb(Uid),
       username = common:sb(Username),
       displayname = common:sb(DisplayName),
       user_type = common:sb(UserType),
       currency = common:sb(Currency),
       user_edit_details = common:mod_edit_details(#edit_details{}, ReqBy)
      };
%% hack so that users can add themselves
create(Uid, Username, DisplayName, UserType, Currency, Uid) ->
    create(Uid, Username, DisplayName, UserType, Currency, #user{uid = common:sb(Uid)}).

create_userinfo(User, ReqBy) ->
    #user_info{
       internal_uid = User#user.internal_uid,
       displayname = User#user.displayname,
       currency = User#user.currency,
       user_edit_details = common:mod_edit_details(User#user.user_edit_details, ReqBy)
      }.

create_mapping(User, ReqBy) ->
    #user_mapping{
       internal_uid = User#user.internal_uid,
       uid = User#user.uid,
       username = User#user.username,
       user_type = User#user.user_type,
       user_mapping_details = common:mod_edit_details(#edit_details{}, common:sb(ReqBy))
      }.

get({username,Username}) ->
    from_mapping(
      mnesia:dirty_index_read(user_mapping, Username, #user_mapping.username));
get({uid, UserId}) ->
    from_mapping(
      mnesia:dirty_read(user_mapping, UserId));
get({internal_uid, UserId}) ->
    from_mapping(
      mnesia:dirty_index_read(user_mapping, UserId, internal_uid));
get(Id) ->
    from_mapping(mnesia:dirty_read(user_mapping, Id) ++
            mnesia:dirty_index_read(user_mapping, Id, #user_mapping.username) ++
            mnesia:dirty_index_read(user_mapping, Id, #user_mapping.internal_uid)).

count_by_usertype(UserType) ->
    length(mnesia:dirty_index_read(user_mapping, UserType, #user_mapping.user_type)).

from_mapping([UserMapping,UserMapping]) ->
    from_mapping([UserMapping]);
from_mapping([UserMapping]) ->
    [UserInfo] = mnesia:dirty_read(user_info, UserMapping#user_mapping.internal_uid),
    #user{
       internal_uid = UserMapping#user_mapping.internal_uid,
       uid = UserMapping#user_mapping.uid,
       username = UserMapping#user_mapping.username,
       user_type = UserMapping#user_mapping.user_type,
       displayname = UserInfo#user_info.displayname,
       currency = UserInfo#user_info.currency,
       user_edit_details = UserInfo#user_info.user_edit_details
      };
from_mapping([]) ->
    no_such_user;
from_mapping(Any) ->
    lager:error("Got wrong info in from_mapping ~p~n", [Any]).

create_mappingtable() ->
    Res = mnesia:create_table( user_mapping,
                         [ {type, set},
                           {disc_copies, [node()]},
                           {index, [username, user_type, internal_uid]},
                           {attributes, record_info(fields,user_mapping)}]),
    lager:info("Trying to create mapping table with result ~p", [Res]).

create_usertable() ->
    Res = mnesia:create_table( user_info,
                        [ {type, set},
                          {disc_copies, [node()]},
                          {index, []},
                          {attributes, record_info(fields,user_info)}]),
    lager:info("Trying to create user table with result ~p", [Res]).

%% to keep up to old API
update_parts([], User) ->
    User;
update_parts([{?USER, DisplayName}|Rest], User) ->
    update_parts(Rest, User#user{displayname = DisplayName});
update_parts([{?DISPLAYNAME, DisplayName}|Rest], User) ->
    update_parts(Rest, User#user{displayname = DisplayName});
update_parts([{?CURRENCY, Currency}|Rest], User) ->
    update_parts(Rest, User#user{currency = Currency});
update_parts([NotSupported|Rest], User) ->
    lager:info("Users.erl: Not supported variable ~p", [NotSupported]),
    update_parts(Rest, User).

to_proplist(User, Userdata) ->
    case Userdata#user_data.protocol of
        "0.36" ->
            to_proplist_36(User);
        _ ->
            to_proplist_old(User)
    end.

to_proplist_36(User) ->
    Fields = record_info(fields, user),
    DFields = record_info(fields, edit_details),
    [_|Vals] = tuple_to_list(User),
    lists:zipwith(fun (user_edit_details, DY) ->
                          [_|DVals] = tuple_to_list(DY),
                          {<<"user_edit_details">>,
                          ?JSONSTRUCT(lists:zipwith(fun(A,B) ->
                                                {atom_to_binary(A, utf8),B} end,
                                        DFields, DVals))};
                      (X, Y) ->
                          {atom_to_binary(X, utf8),Y} end,
                  Fields,
                  Vals).

to_proplist_old(User) ->
    [{<<"uid">>, User#user.username},
     {<<"user">>, User#user.displayname},
     {<<"usertype">>, User#user.user_type},
     {<<"currency">>, User#user.currency},
     {<<"server_timestamp">>, User#user.user_edit_details#edit_details.last_change}].

from_proplist(List, Userdata) ->
    Uuid = common:binary_uuid(),
    User = #user{
              uid = Uuid,
              username = Uuid,
              user_edit_details =
                  common:mod_edit_details(
                    #edit_details{},
                    Userdata#user_data.user)
             },
    UUser = case Userdata#user_data.protocol of
                "0.36" ->
                    from_proplist36(User, List);
                _ ->
                    from_proplist_old(User#user{
                                        user_type = ?LOCAL_USER,
                                        currency  = ?SWEDISH_CRONA
                                       }, List)
            end,
    TUser = case UUser of %% todo check if user
                #user{ user_type = ?LOCAL_USER } ->
                    UUser#user{ uid = Uuid,
                                username = Uuid
                              };
                _ ->
                    UUser
            end,
    validate_user(TUser).

validate_user(User =
                  #user{ username = Username,
                         user_type = UserType,
                         displayname = Displayname,
                         currency = Currency
                       }) ->
    Errors = case Username of
                 <<"">> ->
                     [{error, empty_username}];
                 undefined ->
                     [{error, undefined_username}];
                 _ -> []
             end,
    Errors1 = case UserType of
                  ?GMAIL_USER ->
                      Errors;
                  ?FACEBOOK_USER ->
                      Errors;
                  ?LOCAL_USER ->
                      Errors;
                  _ ->
                      [{error, unknown_user_type}|Errors]
              end,
    Errors2 = case Displayname of
                  undefined ->
                      [{error, unknown_displayname}|Errors1];
                  <<"">> ->
                      [{error, empty_displayname}|Errors1];
                  _ ->
                      Errors1
              end,
    Currencies = exchangerates_server:countries(),
    Errors3 = case proplists:get_value(Currency, Currencies) of
                  undefined ->
                      [{error, unknown_currency}|Errors2];
                  _ ->
                      Errors2
              end,
    case Errors3 of
        [] ->
            User;
        _ ->
            Errors3
    end;
validate_user(Other) ->
    [{error, Other}].


from_proplist36(User, []) ->
    User;
from_proplist36(User, [{?UID, Uid}| Rest]) ->
    from_proplist36(User#user{uid = ?UID_TO_LOWER(Uid),
                              username = ?UID_TO_LOWER(Uid)}, Rest);
from_proplist36(User, [{?DISPLAYNAME, Displayname}| Rest]) ->
    from_proplist36(User#user{displayname = Displayname}, Rest);
from_proplist36(User, [{?USER_TYPE, UserType}| Rest]) ->
    from_proplist36(User#user{user_type = UserType}, Rest);
from_proplist36(User, [{?CURRENCY,Currency}| Rest]) ->
    from_proplist36(User#user{currency = Currency}, Rest);
from_proplist36(T, [{?ECHO_UUID, _}|Rest]) ->
    from_proplist36(T,Rest);
from_proplist36(_User, [Illegal| _Rest]) ->
    lager:info("unsupported transaction variable ~p~n",[Illegal]),
    unsupported_variable.

from_proplist_old(User, []) ->
    User;
from_proplist_old(User, [{?UID,Uid}|Rest]) ->
    from_proplist_old(User#user{ uid = ?UID_TO_LOWER(Uid),
                                 username = ?UID_TO_LOWER(Uid)}, Rest);
from_proplist_old(User, [{?CURRENCY, Currency} | Rest]) ->
    from_proplist_old(User#user{ currency = Currency }, Rest);
from_proplist_old(User, [{?USER_TYPE,Usertype}|Rest]) ->
    from_proplist_old(User#user{ user_type = Usertype }, Rest);
from_proplist_old(User, [{?ECHO_UUID, _}|Rest]) ->
    from_proplist_old(User#user{}, Rest);
from_proplist_old(User, [{?USER,Displayname}|Rest]) ->
    from_proplist_old(User#user{ displayname = Displayname }, Rest);
from_proplist_old(_User, [Illegal| _Rest]) ->
    lager:info("unsupported transaction variable ~p~n",[Illegal]),
    unsupported_variable.

%% from_proplist(PropList) ->
%%     Uid = proplists:get_value(<<"uid">>, PropList),
%%     DisplayName = proplists:get_value(<<"user">>, PropList),
%%     UserType = proplists:get_value(<<"usertype">>, PropList),
%%     Currency = proplists:get_value(<<"currency">>, PropList).

reconstruct(DBName) ->
    dets:traverse(DBName,
                  fun({_Uid,PropList}) ->
                          Uid = proplists:get_value(<<"uid">>, PropList),
                          DisplayName = proplists:get_value(<<"user">>, PropList),
                          UserType = proplists:get_value(<<"usertype">>, PropList),
                          Currency = proplists:get_value(<<"currency">>, PropList),
                          ServerTimestamp = proplists:get_value(<<"server_timestamp">>, PropList),
                          User = create(Uid, Uid, DisplayName, UserType, Currency, Uid),
                          UUID = User#user.internal_uid,
                          Edits = #edit_details{
                                     created_at = ServerTimestamp,
                                     created_by = UUID,
                                     last_change = ServerTimestamp,
                                     last_changed_by = UUID
                                    },
                          UserMapping = #user_mapping{
                                           internal_uid = User#user.internal_uid,
                                           uid = User#user.uid,
                                           username = User#user.username,
                                           user_type = User#user.user_type,
                                           user_mapping_details = Edits
                                          },
                          UserInfo = #user_info{
                                        internal_uid = User#user.internal_uid,
                                        displayname = User#user.displayname,
                                        currency = User#user.currency,
                                        user_edit_details = Edits
                                       },
                          mnesia:dirty_write(UserMapping),
                          mnesia:dirty_write(UserInfo),
                          continue
                  end).

