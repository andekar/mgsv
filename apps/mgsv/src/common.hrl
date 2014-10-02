-record(edit_details, {
          created_at = common:get_timestamp() :: integer(), %% datetime created
          created_by  :: binary(), %% server id of creator
          last_change = common:get_timestamp() :: integer(), %% datetime last edit
          last_changed_by :: binary() %% server id of editor
         }).

-record(user,
        { internal_uid = common:binary_uuid() :: binary(),
          uid :: binary(),
          username :: binary(),
          user_type :: binary(),
          displayname :: binary(),
          currency :: binary(),
          user_edit_details :: #edit_details{}
        }).

-record(org_transaction,
        { amount :: float(),
          currency :: binary()
        }).

-record(transaction,
        { transaction_id = common:binary_uuid() :: binary(),
          paid_by :: binary(),
          paid_by_username :: binary(), %%for now....
          paid_for :: binary(),
          paid_for_username :: binary(), %% for now....
          amount :: float(),
          reason = <<"">> :: binary(),
          timestamp :: integer(),
          server_timestamp = common:get_timestamp() :: integer(),
          currency :: binary(),
          org_transaction = #org_transaction{} :: #org_transaction{},
          edit_details = #edit_details{} :: #edit_details{}
        }).

-record(debt,
        { id  = common:binary_uuid() :: binary(),
          uid1 :: binary(),
          uid1_username :: binary(), %% for now
          uid2 :: binary(),
          uid2_username :: binary(),
          amount :: float(),
          currency :: binary(),
          edit_details = #edit_details{} :: #edit_details{}
        }).

-record(user_data,
        { os = android,
          version = "0.1",
          username = <<"">>,
          id = <<"">>,
          user_type = <<"">>,
          user = no_such_user:: #user{} | no_such_user,
          expected_server_version = <<"">>
        }).
