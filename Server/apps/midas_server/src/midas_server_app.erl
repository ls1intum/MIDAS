%%%-------------------------------------------------------------------
%% @doc tcp_server public API
%% @end
%%%-------------------------------------------------------------------

-module(midas_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

ensure_subdirs([]) ->
	ok;
ensure_subdirs([Dir|T]) ->
	filelib:ensure_dir(ms:path(Dir)),
	ensure_subdirs(T).

start(_StartType, _StartArgs) ->
	%% ensure directories at startup
	ensure_subdirs([
		"upload/",
		"upload/raw/",
		"upload/unpacked/",
		"upload/inferred/",
		"json/",
		"segues/",
		"views/",
		"actions/"
	]),
	%% start link
    midas_server_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
