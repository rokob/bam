-module(bam_lib).

-export([now_milliseconds/0]).
-export([bin_to_hex/1]).

%% API

now_milliseconds() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

bin_to_hex(B) ->
  bam_lib_bin_to_hex:bin_to_hex(B).
