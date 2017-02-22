-module(ctemplate).
-compile(export_all).

start() ->
  spawn(?MODULE, loop, []).

rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} ->
      Response
  end.

loop(X) ->
  receive
    Any ->
      io:format("Received:~p~n", [Any]),
      loop(X)
  end.
