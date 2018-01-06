-module(ex3).
-export([gen_bigfile/2, md5_bigfile/1]).

gen_bigfile(FileName, Bytes) ->
  {ok, Fd} = file:open(FileName, [write,binary,raw]),
  write_bigfile(Fd, Bytes),
  file:close(Fd).

write_bigfile(_Fd, N) when is_integer(N), N =< 0 ->
  ok;
write_bigfile(Fd, N) ->
  Data = [random:uniform(255) || _ <- lists:seq(1, 1024)],
  file:write(Fd, Data),
  write_bigfile(Fd, N-1024).

md5_bigfile(FileName) ->
  {ok, Fd} = file:open(FileName, [read,binary,raw]),
  Ctx = erlang:md5_init(),
  md5_accu(Fd, Ctx),
  file:close(Fd),
  erlang:md5_final(Ctx).

md5_accu(Fd, Ctx) ->
  case file:read(Fd, 1024) of
    {ok, Data} ->
      erlang:md5_update(Ctx, Data),
      md5_accu(Fd, Ctx);
    eof ->
      ok;
    {error, _} ->
      ok
  end.
