-module(bam_ping_check_ping).
-behaviour(bam_ping_check).

-export([init/3, perform/3]).

-define(CHECK_TYPE, ping).

init(_Host, _Port, _Opts) ->
  {ok, []}.

perform(Host, Port, State) ->
  io:format("Performing <~p> for Host: <~s:~B>~n", [?CHECK_TYPE, Host, Port]),
  Url = lists:flatten([Host, $:, integer_to_list(Port)]),
  {ok, {{_Version, Status, ReasonPhrase}, _Headers, _Body}} = httpc:request(get, {Url, []}, [], []),
  Result = {Status, ReasonPhrase},
  {ok, Result, State}.