-module(ex5).
-export([start/0, stop/0, get_md5/1, loop/1]).

-include_lib("kernel/include/file.hrl").

start() ->
	Pid = spawn(?MODULE, loop, [#{}]),
	register(?MODULE, Pid).

stop() ->
	?MODULE ! stop.

get_md5(FileName) ->
	rpc({get, self(), FileName}).

rpc(Cmd) ->
	?MODULE ! Cmd,
	receive
		Response ->
			Response
	end.

get_modified_time(FileName) ->
	case file:read_file_info(FileName) of
		{ok, FileInfo} ->
			#file_info{mtime=ModifiedTime} = FileInfo,
			{ok, ModifiedTime};
		Other ->
			Other
	end.

loop(Cache) ->
	receive
		{get, From, FileName} ->
			{ok, ModifiedTime} = get_modified_time(FileName),
			case maps:is_key(FileName, Cache) of
				true ->
					#{FileName := CacheItem} = Cache,
					{T, MD5HEX} = CacheItem,
					if
						ModifiedTime =< T ->
							From ! {hit, MD5HEX},
							loop(Cache);
						true -> 
							{ok, Data} = file:read_file(FileName),
							MD5HEX2 = erlang:md5(Data),
							NewCache = Cache#{FileName := {ModifiedTime, MD5HEX2}},
							From ! {miss, MD5HEX2},
							loop(NewCache)
					end;
				false ->
					{ok, Data} = file:read_file(FileName),
					MD5HEX = erlang:md5(Data),
					NewCache = Cache#{FileName => {ModifiedTime, MD5HEX}},
					From ! {newcompute, MD5HEX},
					loop(NewCache)
			end;
		stop ->
			ok
	end.
	
