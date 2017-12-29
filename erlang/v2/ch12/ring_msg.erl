-module(ring_msg).
-compile(export_all).

test(N, M) ->
	HeadPid = create_process(N),

	statistics(runtime),
	statistics(wall_clock),
	HeadPid ! {self(), msg, M},
	receive
		test_finish ->
			void
	end,
	{_, Time1} = statistics(runtime),
	{_, Time2} = statistics(wall_clock),

	U1 = Time1 * 1000 / M,
	U2 = Time2 * 1000 / M,
	io:format("msg passing time=~p (~p) microseconds~n", [U1, U2]),

	HeadPid ! die.

create_process(1, NextPid, TailPid) ->
	Pid = spawn(fun() -> loop_head(0, NextPid) end),
	TailPid ! {init, Pid},
	Pid;
create_process(N, NextPid, TailPid) ->
	Pid = spawn(fun() -> loop_middle(NextPid) end),
	create_process(N-1, Pid, TailPid).

create_process(N) ->
	Pid = spawn(fun() -> loop_tail_start() end),
	create_process(N-1, Pid, Pid).

loop_head(TestPid, NextPid) ->
	receive
		{_TestPid, msg, M} ->
			%% io:format("head ~p~n", [M]),
			NextPid ! {msg, M},
			loop_head(_TestPid, NextPid);
		{msg, 1} ->
			%% io:format("head finish~n"),
			TestPid ! test_finish;
		{msg, M} ->
			%% io:format("head ~p~n", [M]),
			NextPid ! {msg, M},
			loop_head(TestPid, NextPid);
		die ->
			NextPid ! die
	end.

loop_tail_start() ->
	receive
		{init, NextPid} ->
			loop_tail(NextPid)
	end.


loop_tail(NextPid) ->
	receive
		{msg, M} ->
			%% io:format("tail ~p~n", [M]),
			NextPid ! {msg, M-1},
			loop_tail(NextPid);
		die ->
			void
	end.

loop_middle(NextPid) ->
	receive
		{msg, M} ->
			%% io:format("middle ~p:~p~n", [self(), M]),
			NextPid ! {msg, M},
			loop_middle(NextPid);
		die ->
			NextPid ! die
	end.
	
