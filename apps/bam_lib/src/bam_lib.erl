-module(bam_lib).

-export([my_func/0]).

%% API

my_func() ->
  io:format("~p~n", [bam_conf:get_val(bam_lib, whoa, answer, 69)]),
  ok().

%% Internals

ok() ->
  ok.
