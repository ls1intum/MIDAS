%%%-------------------------------------------------------------------
%%% @author thomasguenzel
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Jul 2016 13:45
%%%-------------------------------------------------------------------
-module(ms_socket_sup).
-author("thomasguenzel").

-behaviour(supervisor).

%% API
-export([start_link/2]).
-export([start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_child(Server) ->
	supervisor:start_child(Server, []).

start_link(ServerModule, Port) ->
	{ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, [ServerModule, Port]),
	start_child(Pid),
	{ok, Pid}.


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([ServerModule,Port]) ->
	SockOpts = [binary,
		{active, false},
		{packet, 0},
		{reuseaddr, true}],
	{ok, LSock} = gen_tcp:listen(Port, SockOpts),
	Server = {ServerModule, {ServerModule, start_link,
		[LSock]},
		temporary, brutal_kill, worker, [ServerModule]},
	RestartStrategy = {simple_one_for_one, 0, 1},
	lager:info("~p listening on port ~p", [ServerModule,Port]),
	{ok, {RestartStrategy, [Server]}}.

