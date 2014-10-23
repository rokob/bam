-module(bam_ping_worker_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/5]).

-export([init/1]).

-define(SERVER, ?MODULE).

% API

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Host, Port, Interval, CheckMod, CheckOpts) ->
  supervisor:start_child(?SERVER, [Host, Port, Interval, CheckMod, CheckOpts]).

% Callbacks

init([]) ->
  Element = {bam_ping_worker, {bam_ping_worker, start_link, []},
             temporary, brutal_kill, worker, [bam_ping_worker]},
  Children = [Element],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
