%%%-------------------------------------------------------------------
%%% @author thomasguenzel
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Aug 2016 22:24
%%%-------------------------------------------------------------------
-module(ms_ctx_activity).
-author("thomasguenzel").

%% API
-export([infer_activity/1]).


infer_activity(#{activity_context := ActContext} = Rec) when is_map(ActContext)->
	Rec#{activity_context => extract_activities(ActContext)};
infer_activity(Rec) ->
	Rec.

extract_activities(#{
	confidence := _Confidence,
	unknown := Unknown,
	stationary := Stationary,
	walking := Walking,
	running := Running,
	automotive := Automotive,
	cycling := Cycling} = ActContext) ->
	C = conclude_activity(Unknown, Stationary, Walking, Running, Automotive, Cycling),
	ActContext#{concluded => C}.

conclude_activity(_Unknown, _Stationary, _Walking, _Running, _Driving, 1) ->
	cycling;
conclude_activity(_Unknown, _Stationary, _Walking, _Running, 1, _Cycling) ->
	driving;
conclude_activity(_Unknown, _Stationary, _Walking, 1, _Driving, _Cycling) ->
	running;
conclude_activity(_Unknown, _Stationary, 1, _Running, _Driving, _Cycling) ->
	walking;
conclude_activity(_Unknown, 1, _Walking, _Running, _Driving, _Cycling) ->
	stationary;
conclude_activity(_Unknown, _Stationary, _Walking, _Running, _Driving, _Cycling) ->
	unknown.
