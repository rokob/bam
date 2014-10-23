-module(bam_ping_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type, Depends), {I, {I, start_link, []}, permanent, 2000, Type, Depends}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Children = [
    ?CHILD(bam_ping_worker_sup, supervisor, [bam_ping_worker_sup, bam_ping_worker]),
    ?CHILD(bam_ping_event, worker, [bam_ping_event])
  ],
  RestartStrategy = {one_for_one, 4, 3600},
  {ok, {RestartStrategy, Children}}.
