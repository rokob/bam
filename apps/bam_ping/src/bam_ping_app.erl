-module(bam_ping_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  case bam_ping_sup:start_link() of
    {ok, Pid} ->
      bam_ping_event_logger:add_handler(),
      {ok, Pid};
    ignore ->
      ignore;
    {error, Error} ->
      {error, Error}
  end.

stop(_State) ->
  bam_ping_event_logger:delete_handler(),
  ok.
