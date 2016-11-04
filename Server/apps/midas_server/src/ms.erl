%%%-------------------------------------------------------------------
%%% @author thomasguenzel
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Jul 2016 19:17
%%%-------------------------------------------------------------------
-module(ms).
-author("thomasguenzel").

%% API
-export([u/0]).
-export([path/0,path/1]).

u() ->
	ms_unpacker:sample().

path() ->
	{ok, Dir} = application:get_env(midas_server, root_dir),
	Dir.

path(Subpath) ->
	path() ++ Subpath.
