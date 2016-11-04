%%%-------------------------------------------------------------------
%%% @author thomasguenzel
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jun 2016 13:31
%%%-------------------------------------------------------------------
-module(ms_unpacker).
-author("thomasguenzel").

%% API
-export([spawn/1,run/0]).

spawn(Filename) ->
	spawn(?MODULE, run, []) ! Filename.

run() ->
	receive
		Filename -> unpack(Filename)
	end.

parse_data(Data) ->
	{ok, Context, Rest} = parseContext(#{}, sets:from_list([device]), Data),
	Parsed = parse(#{type => session, touches => [], actions => [], segues => []},Rest),
	maps:merge(Parsed, Context).

write_to_json(Parsed, Filename) ->
	{Time,Json} = timer:tc(jiffy,encode, [Parsed, [pretty]]),
	file:write_file(ms:path("json/" ++ Filename ++ ".json"), Json),
	Time.

write_to_msgpack(Parsed, Filename) ->
	{Time,Msgpack} = timer:tc(msgpack, pack, [Parsed]),
	file:write_file(ms:path("upload/unpacked/" ++ Filename ++ ".msgpack"), Msgpack),
	Time.



unpack(Filename) ->
	{ok, Data} = file:read_file(ms:path("upload/raw/" ++ Filename ++ ".bin")),

	Parsed = parse_data(Data),

	JTime = write_to_json(Parsed, Filename),
	MTime = write_to_msgpack(Parsed, Filename),

	lager:info("Finished unpacking ~s to json (~Bus) and msgpack (~Bus)",[Filename,JTime,MTime]),
	ms_postprocess:spawn(Filename, Parsed),
	ok.

%% Recording
parse(#{touches := OldTouches} = Current, <<16#10,N/little-integer, VCName:N/binary,Rest/binary>>) ->
	{ok, Context, GestureRest} = parseContext(#{}, sets:from_list([device_state,time,location,activity,motion]), Rest),
	{ok, Gestures, NewRest} = parseGesture([], GestureRest),
	Gest = #{type => touch_recording, viewIdentifier => VCName, gestures => Gestures},
	E = maps:merge(Gest, Context),
	parse(Current#{touches => lists:append(OldTouches, [E])}, NewRest);
parse(#{actions := OldActions} = Current,
	<<16#60,
		ViewN/little-integer, View:ViewN/binary,
		SelN/little-integer, Sel:SelN/binary,
		KeyN/little-integer, Key:KeyN/binary,
		Rest/binary>>) ->
	{ok, Context, NewRest} = parseContext(#{}, sets:from_list([device_state,time,location,activity,motion]), Rest),
	Actn = #{type => action, viewIdentifier => View, selector => Sel, key => Key},
	E = maps:merge(Actn,Context),
	parse(Current#{actions => lists:append(OldActions,[E])}, NewRest);
parse(#{segues := OldSegues} = Current,
	<<16#61,
		SegueN/little-integer, Segue:SegueN/binary,
		SrcN/little-integer, Src:SrcN/binary,
		DstN/little-integer, Dst:DstN/binary,
		KeyN/little-integer, Key:KeyN/binary,
		Rest/binary>>) ->
	{ok, Context, NewRest} = parseContext(#{}, sets:from_list([device_state,time,location,activity,motion]), Rest),
	Seg = #{type => segue, name => Segue, sourceViewIdentifier => Src, destinationViewIdentifier => Dst, senderKey => Key},
	E = maps:merge(Seg, Context),
	parse(Current#{segues => lists:append(OldSegues,[E])}, NewRest);
parse(Current, <<>>) ->
	Current.

%% Gesture
atomIf(0, _Atom) ->
	[];
atomIf(1, Atom) ->
	[Atom].

parseTouchRecogs(<<_:1,LongPress:1,ScreenEdge:1,Pan:1,Swipe:1,Rotation:1,Pinch:1,Tap:1>>) ->
	atomIf(LongPress, long_press) ++ atomIf(ScreenEdge, screen_edge) ++ atomIf(Pan, pan) ++
		atomIf(Swipe, swipe) ++ atomIf(Rotation, rotation) ++ atomIf(Pinch, pinch) ++ atomIf(Tap, tap).

parseGesture(Current, <<16#20,Flags:1/binary,TimeOffset/little-float, Rest/binary>>) ->
	Recogs = parseTouchRecogs(Flags),
	{ok, Context, NewRest} = parseContext(#{}, sets:from_list([activity,motion]), Rest),
	{ok, TouchSequence, NewRest2} = parseTouchSequence([], NewRest),
	%E = {[{type, gesture}, {timeOffset, TimeOffset}, {gestureRecognizers, Recogs}] ++ Context ++ [{touchSequence, TouchSequence}]},
	Gest = #{type => gesture, timeOffset => TimeOffset, gestureRecognizers => Recogs},
	Seq = #{touchSequence => TouchSequence},
	E = maps:merge(maps:merge(Gest,Context),Seq),
	parseGesture(lists:append(Current,[E]), NewRest2);
parseGesture(Current, <<Rest/binary>>) ->
	{ok, Current, Rest}.


%% Touch Sequence
parseTouchSequence(Current, <<16#30, TimeOffset/little-float, Rest/binary>>) ->
	{ok, Sequence, NewRest} = parseTouches([], Rest),
	E = #{type => touch_sequence, timeOffset => TimeOffset, touches => Sequence},
	parseTouchSequence(lists:append(Current,[E]), NewRest);
parseTouchSequence(Current, <<Rest/binary>>) ->
	{ok, Current, Rest}.



parseTouches(Current, <<16#40, LocX:32/little-float, LocY:32/little-float, TimeOffset:64/little-float, Rest/binary>>) ->
	E = #{type => touches, locationX => LocX, locationY => LocY, timeOffset => TimeOffset},
	parseTouches(lists:append(Current,[E]), Rest);
parseTouches(Current, Rest) ->
	{ok, Current, Rest}.


%% Context

parseContext(Current, ValidTypes,
	<<16#80,
		InterfaceIdiom/little-integer,
		ScreenWidth:16/little-integer,
		ScreenHeight:16/little-integer,
		ModelN/little-integer,
		Model:ModelN/binary,
		SysNameN/little-integer,
		SysName:SysNameN/binary,
		SysVersN/little-integer,
		SysVers:SysVersN/binary,
		AppVersN/little-integer,
		AppVers:AppVersN/binary,
		Rest/binary>>) ->
	case sets:is_element(device, ValidTypes) of
		true ->
			E = #{device_context => #{
				idiom => convertIdiom(InterfaceIdiom),
				screen => #{width => ScreenWidth, height => ScreenHeight},
				model => Model,
				system_name => SysName,
				system_version => SysVers,
				app_version => AppVers}},
			parseContext(maps:merge(Current,E), ValidTypes, Rest);
		false ->
			parseContext(Current, ValidTypes, Rest)
	end;
parseContext(Current, ValidTypes,
	<<16#81,
		Orientation/little-integer,
		BatteryState/little-integer,
		BatteryLevel/little-integer,
		NetworkStatus/little-integer,
		Rest/binary>>) ->
	case sets:is_element(device_state, ValidTypes) of
		true ->
			E = #{device_state_context => #{
				orientation => convertOrientation(Orientation),
				battery => #{state => convertBatteryState(BatteryState), level => BatteryLevel},
				network => convertNetworkStatus(NetworkStatus)
				}
			},
			parseContext(maps:merge(Current,E), ValidTypes, Rest);
		false ->
			parseContext(Current, ValidTypes, Rest)
	end;
parseContext(Current, ValidTypes,
	<<16#90,
		TimezoneOffset:32/little-signed-integer,
		Timestamp/little-float,
		TimezoneN/little-integer,
		Timezone:TimezoneN/binary,
		LocaleN/little-integer,
		Locale:LocaleN/binary,
		Rest/binary>>) ->
	case sets:is_element(time, ValidTypes) of
		true ->
			E = #{time_context => #{
				timestamp => Timestamp,
				timezone_offset => TimezoneOffset,
				timezone => Timezone,
				locale => Locale}},
			parseContext(maps:merge(Current,E), ValidTypes, Rest);
		false ->
			parseContext(Current, ValidTypes, Rest)
	end;
parseContext(Current, ValidTypes,
	<<16#C0,
		AttR/little-float,AttP/little-float, AttY/little-float,
		RateX/little-float, RateY/little-float, RateZ/little-float,
		GravX/little-float, GravY/little-float, GravZ/little-float,
		UserX/little-float, UserY/little-float, UserZ/little-float,
		Rest/binary>>) ->
	case sets:is_element(motion, ValidTypes) of
		true ->
			Att = #{roll => AttR, pitch => AttP, yaw => AttY},
			Rate = #{x => RateX, y => RateY, z => RateZ},
			Grav = #{x => GravX, y => GravY, z => GravZ},
			User = #{x => UserX, y => UserY, z => UserZ},
			Fixed = #{
				attitude => Att,
				rotation_rate => Rate,
				gravity => Grav,
				user_acceleration => User},
			E = #{motion_context => Fixed},
			parseContext(maps:merge(Current,E), ValidTypes, Rest);
		false ->
			parseContext(Current, ValidTypes, Rest)
	end;
parseContext(Current, ValidTypes,
	<<16#C1,
		Latitude/little-float,
		Longitude/little-float,
		Altitude/little-float,
		HAcc/little-float,
		VAcc/little-float,
		Speed/little-float,
		Course/little-float,
		Rest/binary>>) ->
	case sets:is_element(location, ValidTypes) of
		true ->
			Fixed = #{
				latitude => Latitude,
				longitude => Longitude,
				altitude => Altitude,
				horizontal_accuracy => HAcc,
				vertical_accuracy => VAcc,
				speed => Speed,
				course => Course},
			E = #{location_context => Fixed},
			parseContext(maps:merge(Current,E), ValidTypes, Rest);
		false ->
			parseContext(Current, ValidTypes, Rest)
	end;
parseContext(Current, ValidTypes,
	<<16#C2,
		Confidence:2/little-integer,
		Unknown:1/little-integer,
		Stationary:1/little-integer,
		Walking:1/little-integer,
		Running:1/little-integer,
		Automotive:1/little-integer,
		Cycling:1/little-integer,
		Rest/binary>>) ->
	case sets:is_element(activity, ValidTypes) of
		true ->
			Context = #{
				confidence => Confidence,
				unknown => Unknown,
				stationary => Stationary,
				walking => Walking,
				running => Running,
				automotive => Automotive,
				cycling => Cycling},
			E = #{activity_context => Context},
			parseContext(maps:merge(Current,E), ValidTypes, Rest);
		false ->
			parseContext(Current, ValidTypes, Rest)
	end;
parseContext(Current, _ValidTypes, Rest) ->
	{ok, Current, Rest}.

convertOrientation(1) -> portrait;
convertOrientation(2) -> portrait_upside_down;
convertOrientation(3) -> landscape_left;
convertOrientation(4) -> landscape_right;
convertOrientation(5) -> face_up;
convertOrientation(6) -> face_down;
convertOrientation(_) -> unknown.

convertIdiom(1) -> phone;
convertIdiom(2) -> pad;
convertIdiom(3) -> tv;
convertIdiom(4) -> carplay;
convertIdiom(_) -> unknown.

convertBatteryState(1) -> unplugged;
convertBatteryState(2) -> charging;
convertBatteryState(3) -> full;
convertBatteryState(_) -> unknown.

convertNetworkStatus(0) -> not_reachable;
convertNetworkStatus(1) -> wifi;
convertNetworkStatus(2) -> cellular;
convertNetworkStatus(_) -> unknown.
