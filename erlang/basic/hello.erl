% escript hello.erl
-export([main/1]).

main([YourName]) ->
  io:format("hello world 1~n"),
  io:format("hello ~s~n", [YourName]);
main([YourName, Age]) ->
  io:format("hello world 2~n"),
  io:format("hello ~s, age ~s~n", [YourName, Age]);
main(_) ->
  usage().

usage() ->
  io:format("usage: ~s name [age]~n", [filename:basename(escript:script_name())]),
  halt(1).
