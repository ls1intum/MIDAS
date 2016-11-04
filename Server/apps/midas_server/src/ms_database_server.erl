%%%-------------------------------------------------------------------
%%% @author thomasguenzel
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2016 23:38
%%%-------------------------------------------------------------------
-module(ms_database_server).
-author("thomasguenzel").

%% API
-export([start_link/0,add_recording/1,next_recordings_id/0,
	get_weatherdata/3,add_weatherdata/5]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {conn}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


add_recording(UUID) ->
	gen_server:cast(?MODULE, {add_recording, UUID}).

next_recordings_id() ->
	gen_server:call(?MODULE, next_rec_id).

get_weatherdata(Lat, Lon, Time) ->
	gen_server:call(?MODULE, {get_weatherdata, Lat, Lon, Time}).

add_weatherdata(Lat, Lon, Time, Cond, Temp) ->
	gen_server:call(?MODULE, {add_weatherdata, Lat, Lon, Time, Cond, Temp}).


init([]) ->
	{ok, #state{}, 0}.

handle_call(next_rec_id, _From, #state{conn = C} = State) ->
	{ok, BinId} = eredis:q(C, ["INCR", "recordings:id"]),
	Id = binary_to_integer(BinId),
	{reply, {ok, Id}, State};
handle_call({add_weatherdata, Lat, Lon, Time, Cond, Temp}, _From, #state{conn = C} = State) ->
	Key = "w:" ++ integer_to_list(Time),
	Id = internal_add_weatherdata(C, Cond, Temp, Time),
	Res = eredis:q(C, ["GEOADD", Key, float_to_list(Lon,[{decimals, 7}]), float_to_list(Lat,[{decimals, 7}]), Id]),
	{reply, {ok, Id}, State};
handle_call({get_weatherdata, Lat, Lon, Time}, _From, #state{conn = C} = State) ->
	Resp = internal_get_weatherdata(C, float_to_list(Lat,[{decimals, 7}]), float_to_list(Lon,[{decimals, 7}]), Time),
	{reply, Resp, State};
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast({add_recording, UUID}, #state{conn = C} = State) when is_integer(UUID) ->
	eredis:q(C, ["SADD", "recordings", integer_to_list(UUID)]),
	{noreply, State};
handle_cast(Msg, State) ->
	lager:warning("Unknown cast: ~p",[Msg]),
	{noreply, State}.

handle_info(timeout, OldState) ->
	case eredis:start_link("redis",6379) of
		{ok, Some} ->
			C = Some,
			State = #state{conn = C},
			lager:info("Connected to Redis"),
			{noreply, State};
		_ ->
			lager:info("Couldn't connect to Redis"),
			{stop, redis_offline, OldState}
	end;
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


get_next_weather_id(Conn) ->
	{ok, BinId} = eredis:q(Conn, ["INCR", "wd:id"]),
	Id = binary_to_integer(BinId),
	Id.

internal_add_weatherdata(C, Cond, Temp, Time) ->
	RawId = get_next_weather_id(C),
	Id = "wd:" ++ integer_to_list(RawId),
	eredis:q(C, ["HMSET", Id,
		"condition", Cond,
		"temperature", float_to_list(Temp, [{decimals, 2}]),
		"time", integer_to_list(Time)]),
	Id.


internal_get_weatherdata(C, Lat, Lon, Time) ->
	Key = "w:" ++ integer_to_list(Time),
	case eredis:q(C, ["GEORADIUS", Key, Lon, Lat, "20", "km", "COUNT", "1", "ASC"]) of
		{ok, []} ->
			{error, no_data_available};
		{ok, [DataKey]} ->
			{ok, [CondB, TempB, TimeB]} = eredis:q(C, ["HMGET", DataKey, "condition", "temperature", "time"]),
			D = #{condition => binary_to_list(CondB), temperature => binary_to_float(TempB), time => binary_to_integer(TimeB)},
			{ok, D};
		Sth ->
			{error, Sth}
	end.



