-module(myfile).
-export([consult/1]).

consult(File) ->
	case file:open(File, read) of
		{ok, S} ->
			Val = consult1(S),
			file:close(S),
			{ok, Val};
		{error, Why} ->
			{error, Why}
	end.

consult1(S) ->
	case io:read(S, '') of
		{ok, Term} -> [Term|consult1(S)];
		eof        -> [];
		Error      -> Error
	end.

