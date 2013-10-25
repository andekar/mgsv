-module(mgsv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([]) ->
%    Lager = ?CHILD(mgsv_lager_sup, supervisor),
    MgsvServer = ?CHILD(mgsv_server, worker),
    WebMachine = ?CHILD(machine_sup, supervisor),
    PayServer = ?CHILD(pay_server, worker),
    PayPushNotification = ?CHILD(pay_push_notification, worker),
    ValidateUsers = ?CHILD(validate_user, worker),
    Processes = [WebMachine, MgsvServer, PayServer, PayPushNotification, ValidateUsers],
    {ok, { {one_for_one, 6000, 1}, Processes} }.
