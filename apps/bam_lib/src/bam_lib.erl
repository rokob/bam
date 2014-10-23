-module(bam_lib).

-export([my_func/0, now_milliseconds/0]).

%% API

my_func() ->
  io:format("~p~n", [bam_conf:get_val(bam_lib, whoa, answer, 69)]),
  ok().

now_milliseconds() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

%% Internals

ok() ->
  ok.
