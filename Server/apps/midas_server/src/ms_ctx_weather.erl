%%%-------------------------------------------------------------------
%%% @author thomasguenzel
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Aug 2016 05:13
%%%-------------------------------------------------------------------
-module(ms_ctx_weather).
-author("thomasguenzel").

%% API
-export([infer/1,get_weather/0]).

-define(API_KEY, "0657c255112874f4e0fe8a157317dc34").

infer(#{location_context := LocCtx, time_context := TimeCtx} = Rec) ->
	#{unix := Time} = TimeCtx,
	{ok, Weather} = infer_weather(LocCtx, Time),
	Rec#{weather_context => Weather};
infer(Rec) ->
	Rec.

infer_weather(#{longitude := Lon, latitude := Lat} = LocCtx, Time) ->
	WCtx = get_weather(Lat, Lon, Time),
	{ok, WCtx}.

%{status_line(), headers(), Body} | {status_code(), Body} | request_id()

parse_response({_StatusLine, _Headers, Body}) ->
	jiffy:decode(Body, [return_maps]);
parse_response({_StatusCode, Body}) ->
	jiffy:decode(Body, [return_maps]).


get_weather() ->
	%Time = "2016-08-01T10:00:32+00:00",
	Time = 1470000400,
	Lon = 11.55800670,
	Lat = 48.14483530,
	get_weather(Lat, Lon, Time).


get_weather(Lat, Lon, TimeRaw) ->
	Time= TimeRaw - (TimeRaw rem 3600),
	case ms_database_server:get_weatherdata(Lat, Lon, Time) of
		{ok, Resp} ->
			#{condition := Cond} = Resp,
			Resp#{condition => simplify_condition(Cond)};
		{error, Reason} ->
			lager:info("Couldn't get weather: ~p",[Reason]),
			get_weather_remote(Lat, Lon, Time)
	end.

get_weather_remote(Lat, Lon, Time) ->
	lager:info("Time ~p",[Time]),
	IsoTime = iso8601:format(sec_to_datetime(Time)),
	Url = lists:flatten(io_lib:format("https://api.forecast.io/forecast/~s/~f,~f,~s?units=si&exclude=daily,minutely",[?API_KEY,Lat,Lon,IsoTime])),
	{ok,Response} = httpc:request(Url),
	Parsed = parse_response(Response),
	Extracted = extract_hourly(Lat, Lon, Parsed),
	Rounded = Time - (Time rem 3600),
	find_matching_time(Extracted, Rounded).

extract_hourly(Lat, Lon, Resp) ->
	#{<<"hourly">> := Hourly} = Resp,
	#{<<"data">> := Hours} = Hourly,
	lists:map(fun (A) -> extract_hour(Lat, Lon, A) end, Hours).

extract_hour(Lat, Lon, Hour) ->
	#{<<"icon">> := IconB,
		<<"temperature">> := TempB,
		<<"time">> := TimeB} = Hour,
	Icon = binary_to_list(IconB),
	Temp = float(TempB),
	Time = abs(TimeB),
	ms_database_server:add_weatherdata(Lat, Lon, Time, Icon, Temp),
	#{condition => Icon, temperature => Temp, time => Time, datetime => sec_to_datetime(Time)}.

find_matching_time([], _Time) ->
	error;
find_matching_time([Hour|T], Time) ->
	#{time := Time2} = Hour,
	if
		Time == Time2 ->
			#{condition := Icon, temperature := Temp} = Hour,
			#{condition => simplify_condition(Icon), temperature => Temp, time => Time2};
		true ->
			find_matching_time(T, Time)
	end.


sec_to_datetime(Secs) ->
	BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
	Seconds       = BaseDate + Secs,
	calendar:gregorian_seconds_to_datetime(Seconds).

simplify_condition(Cond) when is_binary(Cond) ->
	simplify_condition(list_to_binary(Cond));
simplify_condition(Cond) when is_list(Cond) ->
	Mapping = [{"clear",clear},{"cloudy",cloudy},{"rain",rainy},{"snow",rainy},{"sleet",rainy}],
	cond_map(Cond, Mapping).

cond_map(Cond, []) ->
	unknown;
cond_map(Cond, [{Search, Result}|T]) ->
	Idx = string:str(Cond, Search),
	if
		Idx > 0 -> Result;
		true -> cond_map(Cond, T)
	end.

