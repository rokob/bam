-module(bam_lib).

-export([
  now_milliseconds/0,
  days_from_now/1,
  hours_from_now/1,
  minutes_from_now/1,
  seconds_from_now/1
  ]).

-export([bin_to_hex/1]).

%% API

now_milliseconds() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

days_from_now(N)    -> now_offset(N*24*60*60).
hours_from_now(N)   -> now_offset(N*60*60).
minutes_from_now(N) -> now_offset(N*60).
seconds_from_now(N) -> now_offset(N).

bin_to_hex(B) ->
  bam_lib_bin_to_hex:bin_to_hex(B).

%% Private

now_offset(N) ->
  Now = now_milliseconds(),
  now_offset(Now, N).

now_offset(Now, N) ->
  Now + N*1000.