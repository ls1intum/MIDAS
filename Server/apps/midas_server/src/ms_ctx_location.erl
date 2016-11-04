%%%-------------------------------------------------------------------
%%% @author thomasguenzel
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Aug 2016 07:18
%%%-------------------------------------------------------------------
-module(ms_ctx_location).
-author("thomasguenzel").

%% API
-export([infer/1]).

infer(#{type := touch_recording} = Rec) ->
	#{gestures := Gestures} = Rec,
	Activity = activity_from_gestures(Gestures),
	SimulatedRec = Rec#{activity_context => Activity},
	NewRec = infer_env(SimulatedRec),
	UnsimulatedRec = maps:remove(activity_context, NewRec),
	UnsimulatedRec;
infer(#{type := segue} = Rec) ->
	infer_env(Rec);
infer(#{type := action} = Rec) ->
	infer_env(Rec);
infer(Rec) ->
	Rec.


infer_env(Rec) ->
	Map = env(Rec),
	Loc = case safe_get(location_context, Rec) of
		      undefined ->
			      #{environment => Map};
		      Ctx ->
				  Ctx#{environment => Map}
	end,
	Rec#{location_context => Loc}.

env(Rec) ->
	Wifi = get_wifi(Rec),
	{Hour, Weekday} = get_time(Rec),
	Battery = get_battery(Rec),
	Activity = get_activity(Rec),
	Condition = get_condition(Rec),
	Accuracy = get_accuracy(Rec),
	inf(Wifi, Hour, Weekday, Battery, Activity, Condition, Accuracy).

inf(Wifi, Hour, Weekday, Battery, Activity, Condition, Accuracy) ->
	Home = home(Wifi, Hour, Weekday, Battery, Activity, Condition, Accuracy),
	Outdoor = outdoor(Wifi, Hour, Weekday, Battery, Activity, Condition, Accuracy),
	Transit = transit(Wifi, Hour, Weekday, Battery, Activity, Condition, Accuracy),
	Office = office(Wifi, Hour, Weekday, Battery, Activity, Condition, Accuracy),
	Max = lists:max([Home,Outdoor,Transit,Office]),
	best(Max, Home, Outdoor, Transit, Office).

best(0, _Home, _Outdoor, _Transit, _Office) ->
	unknown;
best(Max, Home, _Outdoor, _Transit, _Office) when Home == Max ->
	home;
best(Max, _Home, Outdoor, _Transit, _Office) when Outdoor == Max ->
	outdoor;
best(Max, _Home, _Outdoor, Transit, _Office) when Transit == Max ->
	transit;
best(Max, _Home, _Outdoor, _Transit, Office) when Office == Max ->
	office.



home(true, Hour, _Weekday, _Battery, _Activity, _Condition, _Accuracy) when Hour >= 17, Hour =< 9 ->
	4;
home(true, _Hour, false, _Battery, _Activity, _Condition, _Accuracy) ->
	3;
home(true, _Hour, _Weekday, charged, _Activity, _Condition, _Accuracy) ->
	2;
home(true, _Hour, _Weekday, charging, _Activity, _Condition, _Accuracy) ->
	1;
home(_Wifi, _Hour, _Weekday, _Battery, _Activity, _Condition, _Accuracy) ->
	0.

outdoor(false, _Hour, _Weekday, _Battery, stationary, _Condition, Accuracy) when Accuracy =< 20.0 ->
	4;
outdoor(false, _Hour, _Weekday, _Battery, _Activity, _Condition, Accuracy) when Accuracy =< 20.0 ->
	3;
outdoor(false, _Hour, _Weekday, _Battery, stationary, _Condition, _Accuracy) ->
	2;
outdoor(false, _Hour, false, _Battery, _Activity, sunny, _Accuracy) ->
	1;
outdoor(_Wifi, _Hour, _Weekday, _Battery, _Activity, _Condition, _Accuracy) ->
	0.

transit(false, Hour, false, _Battery, stationary, _Condition, _Accuracy) when Hour >= 6, Hour =< 8->
	2;
transit(false, Hour, false, _Battery, stationary, _Condition, _Accuracy) when Hour >= 16, Hour =< 19 ->
	1;
transit(_Wifi, _Hour, _Weekday, _Battery, _Activity, _Condition, _Accuracy) ->
	0.

office(true, Hour, true, _Battery, stationary, _Condition, _Accuracy) when Hour >= 8, Hour =< 17 ->
	0;
office(true, Hour, true, _Battery, _Activity, _Condition, _Accuracy) when Hour >= 8, Hour =< 17->
	0;
office(_Wifi, Hour, true, _Battery, _Activity, _Condition, _Accuracy) when Hour >= 8, Hour =< 17 ->
	1;
office(_Wifi, _Hour, _Weekday, _Battery, _Activity, _Condition, _Accuracy) ->
	0.


safe_get(undefined, _) ->
	undefined;
safe_get(_, undefined) ->
	undefined;
safe_get(Key, Recording) ->
	maps:get(Key, Recording, undefined).
	%case maps:get(Key, Recording) of
	%	{badkey, _} ->
	%		undefined;
	%	Val ->
	%		Val
	%end.

activity_from_gestures([]) ->
	undefined;
activity_from_gestures([Gesture|Remaining]) ->
	case safe_get(activity_context, Gesture) of
		undefined ->
			activity_from_gestures(Remaining);
		ActCtx ->
			ActCtx
	end.


get_wifi(Rec) ->
	case safe_get(network, safe_get(device_state_context, Rec)) of
		wifi ->
			true;
		_ ->
			false
	end.

get_time(Rec) ->
	case safe_get(time_context, Rec) of
		undefined ->
			{undefined, undefined};
		TCtx ->
			Hour = safe_get(hour, TCtx),
			Weekday = safe_get(weekday, TCtx),
			{Hour, Weekday}
	end.

get_battery(Rec) ->
	safe_get(state, safe_get(battery, safe_get(device_state_context, Rec))).

get_activity(Rec) ->
	safe_get(concluded, safe_get(activity_context, Rec)).

get_condition(Rec) ->
	safe_get(condition, safe_get(weather_context, Rec)).

get_accuracy(Rec) ->
	safe_get(horizontal_accuracy, safe_get(location_context, Rec)).
