%%%-------------------------------------------------------------------
%%% @author thomasguenzel
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jul 2016 22:14
%%%-------------------------------------------------------------------
-module(ms_ctx_infer).
-author("thomasguenzel").

%% API
-export([infer/1]).

infer(Parsed) ->
	lists:map(fun traverse/1, Parsed).

infer_time(#{time_context := TimeContext} = Rec) when is_map(TimeContext)->
	NewCtx = ms_ctx_time:extract(TimeContext),
	Rec#{time_context => NewCtx};
infer_time(Rec) ->
	Rec.

infer_ctx(Rec) ->
	R1 = infer_time(Rec),
	R2 = ms_ctx_weather:infer(R1),
	R3 = ms_ctx_activity:infer_activity(R2),
	R4 = ms_ctx_location:infer(R3),
	R4.


traverse(#{type := touch_recording, gestures := Gestures} = Rec) ->
	NewGestures = lists:map(fun traverse/1, Gestures),
	NewRec = infer_ctx(Rec#{gestures => NewGestures}),
	NewRec;
traverse(#{type := gesture} = Rec) ->
	NewRec = infer_ctx(Rec),
	NewRec;
traverse(#{type := segue} = Rec) ->
	NewRec = infer_ctx(Rec),
	NewRec;
traverse(#{type := action} = Rec) ->
	NewRec = infer_ctx(Rec),
	NewRec;
traverse(Rec) ->
	Rec.
