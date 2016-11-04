-module(ms_tracking_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {lsock, socket, buffer = <<>>, type = header, content_remaining = 0, zLib, fd, filename, parent}).

%%%===================================================================
%%% API

start_link(LSock) ->
	gen_server:start_link(?MODULE,
		[LSock, self()], []).


%% HELPERS

init_zlib() ->
	Z = zlib:open(),
	ok = zlib:inflateInit(Z, -15),
	Z.

close_zlib(#state{zLib = undefined}) -> void;
close_zlib(#state{zLib = ZLib}) ->
	zlib:close(ZLib).

close_file(#state{fd = undefined}) -> void;
close_file(#state{fd = Fd}) ->
	file:close(Fd).

uuid_to_list(UUID) ->
	<<A:32,B:16,C:16,D:16,E:48>> = UUID,
	Str =
		integer_to_list(A,16) ++ "-" ++
		integer_to_list(B,16) ++ "-" ++
		integer_to_list(C,16) ++ "-" ++
		integer_to_list(D,16) ++ "-" ++
		integer_to_list(E,16),
	string:to_lower(Str).

open_file(Id) ->
	IDFilename = integer_to_list(Id),
	Filename = ms:path("upload/raw/" ++ IDFilename ++ ".bin"),
	{ok, Fd} = file:open(Filename, [write, binary]),
	{ok, Fd, IDFilename}.


%%%===================================================================
%%% gen_server callbacks

init([LSock, Parent]) ->
	State = #state{lsock = LSock, parent = Parent},
	{ok, State, 0}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({tcp, Sock, Data}, State) when is_binary(Data) ->
	CompleteData = list_to_binary([State#state.buffer, Data]),
	NewState = handle_data(Sock, State#state{buffer = CompleteData}),
	inet:setopts(State#state.socket, [{active,once}]),
	{noreply, NewState};
handle_info({tcp_closed, _Sock}, State) ->
	{stop, normal, State};
handle_info(timeout, #state{lsock = LSock, parent = Parent} = State) ->
	{ok, Socket} = gen_tcp:accept(LSock),
	midas_server_sup:start_child(Parent),
	inet:setopts(Socket,[{active,once}]),
	{noreply, State#state{socket = Socket, type = header, content_remaining = 22}}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


parse_header(<<"M",1/little-integer,Length:32/little-integer,Rest/binary>>) ->
	{ok, {header, Length}, Rest};
parse_header(Rest) ->
	{incomplete, Rest}.

decompress_content(Content, Z, Fd) ->
	try
		Decompressed = zlib:inflate(Z, Content),
		file:write(Fd, Decompressed),
		{ok}
	catch
		_Class:Err  ->
			{error, Err}
	end.

parse_content(Content, 0, _Z, _Fd) ->
	{finished, Content};
parse_content(Content, Length, Z, Fd) when byte_size(Content) >= Length ->
	%io:format("pc1: ~p ~p~n",[byte_size(Content), Length]),
	<<RealContent:Length/binary, Rest/binary>> = Content,
	decompress_content(RealContent, Z, Fd),
	{finished, Rest};
parse_content(Content, Length, Z, Fd) ->
	Size = byte_size(Content),
	Remaining = Length - Size,
	%io:format("pc2 remaining: ~p ~p ~p~n",[Length, Size, Remaining]),
	decompress_content(Content, Z, Fd),
	{cont, Remaining}.



handle_data(Socket, #state{type = header} = State) ->
	NewState = case parse_header(State#state.buffer) of
		{ok, {header, Length}, Rest} ->
			{ok,Id} = ms_database_server:next_recordings_id(),
			ms_database_server:add_recording(Id),
			ZLib = init_zlib(),
			{ok, Fd, Filename} = open_file(Id),
			S = State#state{buffer = Rest, type = content, content_remaining = Length, zLib = ZLib, fd = Fd, filename = Filename},
			handle_data(Socket, S);
		{incomplete, _Rest} ->
			State
	end,
	NewState;
handle_data(Socket, #state{type = content} = State) ->
	#state{content_remaining = Length, zLib = Z, fd = Fd} = State,
	NewState = case parse_content(State#state.buffer, Length, Z, Fd) of
		           {finished, Rest} ->
			           close_file(State),
			           close_zlib(State),
			           ms_unpacker:spawn(State#state.filename),
			           S = State#state{buffer = Rest, type = header, content_remaining = 0, zLib = undefined, fd = undefined, filename = undefined},
			           handle_data(Socket, S);
		           {cont, Remaining} ->
			           State#state{buffer = <<>>, content_remaining = Remaining}
	           end,
	NewState.




