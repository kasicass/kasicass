-module(test1).
-export([f1/0]).

f1() ->
  X = erlang:time(),
  seconds(X).

seconds({_Year, _Month, _Day, Hour, Min, Sec}) ->
  (Hour * 60 + Min)*60 + Sec.
