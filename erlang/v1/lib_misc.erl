-module(lib_misc).

-export([sleep/1]).
-export([flush_buffer/0]).
-export([priority_receive/0]).

sleep(T) ->
  receive
  after T ->
    true
  end.

flush_buffer() ->
  receive
    _Any ->
      flush_buffer()
  after 0 ->
    true
  end.

%% 优先接收 {alarm, X} 消息
%% mailbox 中有很多消息时，性能低
priority_receive() ->
  receive
    {alarm, X} ->
      {alarm, X}
  after 0 ->
    receive
      Any ->
        Any
    end
  end.

