%%%-------------------------------------------------------------------
%%% @author thomasguenzel
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jul 2016 20:47
%%%-------------------------------------------------------------------
-module(ms_postprocess).
-author("thomasguenzel").

%% API
-export([spawn/2]).

folder(Filename) ->
	ms:path("upload/inferred/" ++ Filename ++ "/").


process_touches(_Filename, _DeviceContext, []) ->
	ok;
process_touches(Filename, DeviceContext, Touches) ->
	Processed = ms_ctx_infer:infer(Touches),
	Json = jiffy:encode(Processed, [pretty]),
	file:write_file(folder(Filename) ++ "touches.json",Json),
	ms_persist:touches(Processed,DeviceContext),
	ok.

process_segues(_Filename, _DeviceContext, []) ->
	ok;
process_segues(Filename, DeviceContext, Segues) ->
	Processed = ms_ctx_infer:infer(Segues),
	Json = jiffy:encode(Processed, [pretty]),
	file:write_file(folder(Filename) ++ "segues.json",Json),
	ms_persist:segues(Processed,DeviceContext),
	ok.

process_actions(_Filename, _DeviceContext, []) ->
	ok;
process_actions(Filename, DeviceContext, Actions) ->
	Processed = ms_ctx_infer:infer(Actions),
	Json = jiffy:encode(Processed, [pretty]),
	file:write_file(folder(Filename) ++ "actions.json",Json),
	ms_persist:actions(Processed,DeviceContext),
	ok.

spawn(Filename, Parsed) ->
	#{device_context := DeviceContext, touches := Touches, actions := Actions, segues := Segues} = Parsed,
	filelib:ensure_dir(folder(Filename)),
	spawn(fun () -> process_touches(Filename, DeviceContext, Touches) end),
	spawn(fun () -> process_segues(Filename, DeviceContext, Segues) end),
	spawn(fun () -> process_actions(Filename, DeviceContext, Actions) end),
	%spawn(?MODULE, process_touches, [Filename, DeviceContext, Touches]),
	ok.






