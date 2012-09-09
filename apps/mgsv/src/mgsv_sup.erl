-module(mgsv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([]) ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, Dispatch} = file:consult(filename:join(
                         ["..","..", "priv", "dispatch.conf"])),
    WebConfig = [
                 {ip, Ip},
                 {port, 8000},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           transient, 5000, worker, [mochiweb_socket_server]},
    MgsvServer = ?CHILD(mgsv_server, worker),
    PayServer = ?CHILD(pay_server, worker),
    Processes = [Web, MgsvServer, PayServer],
    {ok, { {one_for_one, 10, 10}, Processes} }.
