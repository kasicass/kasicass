-module(example1).
-export([start/0, stop/0]).
-export([twice/1, sum/2]).

start() ->
	register(example1, spawn(fun() ->
			process_flag(trap_exit, true),
			Port = open_port({spawn, "example1.exe"}, [{packet, 2}]),
			loop(Port)
		end)).

stop() ->
	?MODULE ! stop.

twice(X) -> call_port({twice, X}).
sum(X,Y) -> call_port({sum, X, Y}).

call_port(Msg) ->
	?MODULE ! {call, self(), Msg},
	receive
		{?MODULE, Result} ->
			Result
	end.

loop(Port) ->
	receive
		{call, Caller, Msg} ->
			Port ! {self(), {command, encode(Msg)}},
			receive
				{Port, {data, Data}} ->
					Caller ! {?MODULE, decode(Data)}
			end,
			loop(Port);
		stop ->
			Port ! {self(), close},
			receive
				{Port, closed} ->
					exit(normal)
			end;
		{'EXIT', Port, Reason} ->
			exit({port_terminated, Reason})
	end.

encode({sum, X, Y}) -> [1, X, Y];
encode({twice, X})  -> [2, X].

decode([Int]) -> Int.

