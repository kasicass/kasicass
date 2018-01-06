-module(mycount).
-export([count_characters/1]).

count_characters(Str) ->
	count_characters(Str, #{}).

count_characters([H|T], X) ->
	case maps:find(H, X) of
		{ok, N} ->
			count_characters(T, X#{H := N+1});
		error ->
			count_characters(T, X#{H => 1})
	end;
count_characters([], X) ->
	X.

