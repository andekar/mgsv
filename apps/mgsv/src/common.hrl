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

