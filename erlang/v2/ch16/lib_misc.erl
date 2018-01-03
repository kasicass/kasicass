-module(lib_misc).
-export([dump/2, unconsult/2]).
-export([ls/1]).

dump(File, Term) ->
	Out = File ++ ".tmp",
	io:format("** dumping ot ~s~n", [Out]),
	{ok, S} = file:open(Out, [write]),
	io:format(S, "~p.~n", [Term]),
	file:close(S).

unconsult(File, L) ->
	{ok, S} = file:open(File, write),
	lists:foreach(fun(X) -> io:format(S, "~p.~n", [X]) end, L),
	file:close(S).

-include_lib("kernel/include/file.hrl").
file_size_and_type(File) ->
	case file:read_file_info(File) of
		{ok, Facts} ->
			{Facts#file_info.type, Facts#file_info.size};
		_ ->
			error
	end.

ls(Dir) ->
	{ok, L} = file:list_dir(Dir),
	lists:map(fun(I) -> {I, file_size_and_type(I)} end, lists:sort(L)).

