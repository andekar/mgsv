-module(mgsv_lager_sup).

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

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([]) ->
    Lager = {mgsv_lager,
           {lager, start, []},
           permanent, 5000, supervisor, [lager]},
    Processes = [Lager],
    {ok, { {one_for_one, 6000, 1}, Processes} }.
