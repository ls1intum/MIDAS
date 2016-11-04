%%%-------------------------------------------------------------------
%%% @author thomasguenzel
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Jul 2016 18:38
%%%-------------------------------------------------------------------
-module(ms_ctx_time).
-author("thomasguenzel").

%% API
-export([extract/1]).

sec_to_datetime(Secs) ->
	BaseDate      = calendar:datetime_to_gregorian_seconds({{2001,1,1},{0,0,0}}),
	UnixDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
	Seconds       = BaseDate + Secs,
	UnixTimestamp = Seconds - UnixDate,
	Datetime = calendar:gregorian_seconds_to_datetime(Seconds),
	{UnixTimestamp, Datetime}.

time_of_day({Hour, _Minute, _Second}) when Hour < 5 ->
	night;
time_of_day({Hour, _Minute, _Second}) when Hour < 10 ->
	morning;
time_of_day({Hour, _Minute, _Second}) when Hour < 14 ->
	noon;
time_of_day({Hour, _Minute, _Second}) when Hour < 19 ->
	afternoon;
time_of_day({Hour, _Minute, _Second}) when Hour < 23 ->
	evening;
time_of_day(_) ->
	night.

extract(#{timezone_offset := Offset, timestamp := Timestamp} = Ctx) ->
	Ex = extract(Timestamp, Offset),
	maps:merge(Ex,Ctx).

extract(Timestamp, Offset) ->
	Adjusted = round(Timestamp) + Offset,
	{Unix,Datetime} = sec_to_datetime(Adjusted),
	{Date, Time} = Datetime,
	DayOfWeek = calendar:day_of_the_week(Date),
	IsWeekday = DayOfWeek < 6,
	TimeOfDay = time_of_day(Time),
	{Hour,_,_} = Time,
	#{day_of_week => DayOfWeek, weekday => IsWeekday, time_of_day => TimeOfDay, hour => Hour, unix => Unix}.

