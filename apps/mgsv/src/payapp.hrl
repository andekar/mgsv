%% USERID
-define(UID, <<"uid">>).
-define(UID(Uid), {?UID, Uid}).

%% Uniq uid
-define(UUID, <<"uuid">>).
-define(UUID(Uuid), {?UUID, Uuid}).

%% User 1 id
-define(UID1, <<"uid1">>).
-define(UID1(Uid1), {?UID1, Uid1}).

%% User 2 id
-define(UID2, <<"uid2">>).
-define(UID2(Uid2), {?UID2, Uid2}).

%% Username 1
-define(USER1, <<"user1">>).
-define(USER1(User1), {?USER1, User1}).

%% Username 2
-define(USER2, <<"user2">>).
-define(USER2(User2), {?USER2, User2}).

%% Username
-define(USER, <<"user">>).
-define(USER(User), {?USER, User}).

%% Amount money
-define(AMOUNT, <<"amount">>).
-define(AMOUNT(Amount), {?AMOUNT, Amount}).

%% Reason of debt
-define(REASON, <<"reason">>).
-define(REASON(Reason), {?REASON, Reason}).

%% Time stamp of time when the debt was added
-define(TIMESTAMP, <<"timestamp">>).
-define(TIMESTAMP(TimeStamp), {?TIMESTAMP, TimeStamp}).

%% Status to return to client usually <<"ok">>
-define(STATUS, <<"status">>).
-define(STATUS(Status), {?STATUS, Status}).

%% Create a struct to put the json in
-define(JSONSTRUCT, struct).
-define(JSONSTRUCT(JSons), {?JSONSTRUCT, JSons}).

%% Create a debt struct
-define(DEBT, <<"debt">>).
-define(DEBT(Debt), {?DEBT, ?JSONSTRUCT(Debt)}).

%% approved by user
-define(ADDED_BY, <<"added_by">>).
-define(ADDED_BY(Uid), {?ADDED_BY, Uid}).

%% databases

%% {Uuid, {Uuid1, Uuid2}, TimeStamp, Reason, Amount}
-define(DEBT_RECORD, debt_record).
-define(DEBT_RECORD(Props), proplists:get_value(?DEBT_RECORD, Props)).

%% DEBTS {{Uid1, Uid2}, Amount}
-define(DEBTS, debts).
-define(DEBTS(Props), proplists:get_value(?DEBTS, Props)).

%% {Uuid, Username}
-define(USERS, users).
-define(USERS(Props), proplists:get_value(?USERS, Props)).

%% {Uuid, [{?APPROVED_DEBTS, [Uids]}, {?NOT_APPROVED_DEBTS, [Uids]}] }
%% That is Uuid = userid, Uids = debt_ids
-define(DEBT_APPROVAL_TRANSACTIONS, debt_approval_transactions).
-define(DEBT_APPROVAL_TRANSACTIONS(Props), proplists:get_value(?DEBT_APPROVAL_TRANSACTIONS, Props)).

%% property of (not)approved debts
-define(APPROVED_DEBTS, approved_debts).
-define(APPROVED_DEBTS(Props), proplists:get_value(?APPROVED_DEBTS, Props, [])).

-define(NOT_APPROVED_DEBTS, approved_debts).
-define(NOT_APPROVED_DEBTS(Props), proplists:get_value(?NOT_APPROVED_DEBTS, Props, [])).

%% to lower and check @
-define(UID_TO_LOWER(User), list_to_binary(string:to_lower(binary_to_list(User)))).
