%%%-------------------------------------------------------------------
%% @doc spread_process_launcher top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(spread_process_launcher_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ProcessLauncher = {spread_process_launcher, {spread_process_launcher, start_link, []}, permanent, 5000, worker, [spread_process_launcher]},
    {ok, { {one_for_one, 5, 1}, [ProcessLauncher]} }.

%%====================================================================
%% Internal functions
%%====================================================================
