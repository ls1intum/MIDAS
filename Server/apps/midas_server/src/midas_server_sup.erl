%%%-------------------------------------------------------------------
%% @doc tcp_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(midas_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_child(Server) ->
	supervisor:start_child(Server, []).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	TrackingServer = {
		ms_socket_sup, {ms_socket_sup, start_link, [ms_tracking_server, 21237]},
		permanent, 2000, supervisor, [ms_database_server]
	},
	DatabaseServer = {
		ms_database_server, {ms_database_server, start_link, []},
		permanent, 2000, worker, [ms_database_server]
	},
	PersistServer = {
		ms_persist, {ms_persist, start_link, []},
		permanent, 2000, worker, [ms_persist]
	},
	Children = [DatabaseServer,TrackingServer,PersistServer],
	RestartStrategy = {one_for_one, 0, 1},
	{ok, {RestartStrategy, Children}}.


%%====================================================================
%% Internal functions
%%====================================================================
