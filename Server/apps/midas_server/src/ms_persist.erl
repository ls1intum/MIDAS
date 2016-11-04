%%%-------------------------------------------------------------------
%%% @author thomasguenzel
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jul 2016 12:16
%%%-------------------------------------------------------------------
-module(ms_persist).
-author("thomasguenzel").

%% API
-export([start_link/0,touches/2,segues/2,actions/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {conn}).

handle_touches([], _BaseDir) ->
	ok;
handle_touches([H|T], BaseDir) ->
	#{viewIdentifier := ViewID} = H,
	Dir = BaseDir ++ binary_to_list(ViewID) ++ "/",
	filelib:ensure_dir(ms:path(Dir)),
	lager:info("Saving to: ~p",[ms:path(Dir)]),
	{ok,File} = file:open(ms:path(Dir ++ "touches.msgpack"), [write, append, binary]),
	Msgpack = msgpack:pack(H, [{pack_str, from_binary}]),
	file:write(File, Msgpack),
	file:close(File),
	handle_touches(T, BaseDir).


handle_segues([], _BaseDir) ->
	ok;
handle_segues([H|T], BaseDir) ->
	Dir = BaseDir,
	filelib:ensure_dir(ms:path(Dir)),
	lager:info("Saving to: ~p",[ms:path(Dir)]),
	{ok,File} = file:open(ms:path(Dir ++ "segues.msgpack"), [write, append, binary]),
	Msgpack = msgpack:pack(H, [{pack_str, from_binary}]),
	file:write(File, Msgpack),
	file:close(File),
	handle_segues(T, BaseDir).

handle_actions([], _BaseDir) ->
	ok;
handle_actions([H|T], BaseDir) ->
	#{viewIdentifier := ViewID} = H,
	Dir = BaseDir ++ binary_to_list(ViewID) ++ "/",
	filelib:ensure_dir(ms:path(Dir)),
	lager:info("Saving to: ~p",[ms:path(Dir)]),
	{ok,File} = file:open(ms:path(Dir ++ "actions.msgpack"), [write, append, binary]),
	Msgpack = msgpack:pack(H, [{pack_str, from_binary}]),
	file:write(File, Msgpack),
	file:close(File),
	handle_actions(T, BaseDir).


touches(Touches, DeviceContext) ->
	gen_server:cast(?MODULE, {persist_touches, Touches, DeviceContext}).

segues(Segues, DeviceContext) ->
	gen_server:cast(?MODULE, {persist_segues, Segues, DeviceContext}).

actions(Actions, DeviceContext) ->
	gen_server:cast(?MODULE, {persist_actions, Actions, DeviceContext}).

% gen_server

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast({persist_touches, Touches, DeviceContext}, State) ->
	#{screen := Screen} = DeviceContext,
	#{app_version := AppVers} = DeviceContext,
	#{width := W, height := H} = Screen,
	BaseDir = binary_to_list(AppVers) ++ "/" ++ integer_to_list(W) ++ "x" ++ integer_to_list(H) ++ "/",
	handle_touches(Touches, BaseDir),
	{noreply, State};
handle_cast({persist_segues, Segues, DeviceContext}, State) ->
	#{screen := Screen} = DeviceContext,
	#{app_version := AppVers} = DeviceContext,
	#{width := W, height := H} = Screen,
	BaseDir = binary_to_list(AppVers) ++ "/" ++ integer_to_list(W) ++ "x" ++ integer_to_list(H) ++ "/",
	handle_segues(Segues, BaseDir),
	{noreply, State};
handle_cast({persist_actions, Actions, DeviceContext}, State) ->
	#{screen := Screen} = DeviceContext,
	#{app_version := AppVers} = DeviceContext,
	#{width := W, height := H} = Screen,
	BaseDir = binary_to_list(AppVers) ++ "/" ++ integer_to_list(W) ++ "x" ++ integer_to_list(H) ++ "/",
	handle_actions(Actions, BaseDir),
	{noreply, State};
handle_cast(Msg, State) ->
	lager:warning("Unknown cast: ~p",[Msg]),
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.