% @author rokob <wvvwwvw@gmail.com>
% @copyright rokob, 2014
% @doc
%   The Bam Ping API
% @end

-module(bam_ping).

-export([create/2, create/3, create/4,
         change_interval/2, change_check_mod/2,
         stop_checks/1]).

-define(DEFAULT_CHECK_INTERVAL, 60).
-define(DEFAULT_CHECK_MOD, bam_ping_check_ping).

% @spec create(Host :: string(), Port :: integer()) -> {ok, pid()}
% @doc
%   Create a new worker and start it pinging the _Host_ on _Port_
%   using the default time interval of _60_ seconds and the default
%   check type (http ping).
% @end
create(Host, Port) ->
  create(Host, Port, ?DEFAULT_CHECK_INTERVAL).

% @spec create(Host :: string(), Port :: integer(), Interval :: integer()) -> {ok, pid()}
% @doc
%   Create a new worker and start it pinging the _Host_ on _Port_
%   every _Interval_ seconds using the default
%   check type (http ping).
% @end
create(Host, Port, Interval) ->
  create(Host, Port, Interval, ?DEFAULT_CHECK_MOD).

% @spec create(Host :: string(),
%              Port :: integer(),
%              Interval :: integer(),
%              CheckMod :: atom()) -> {ok, pid()}
% @doc
%   Create a new worker and start it pinging the _Host_ on _Port_
%   every _Interval_ seconds using the check defined by _CheckMod_.
% @end
create(Host, Port, Interval, CheckMod) ->
  bam_ping_worker:create(Host, Port, Interval, CheckMod).

% @spec change_interval(Pid :: pid(), NewInterval :: integer()) -> ok
% @doc
%   Make the checks occur every _NewInterval_ seconds.
% @end
change_interval(Pid, NewInterval) ->
  gen_server:cast(Pid, {change_interval, NewInterval}).

% @spec change_check_mod(Pid :: pid(), NewCheckMod :: atom()) -> ok
% @doc
%   Make the checks use _NewCheckMod_ to define the type of check performed.
% @end
change_check_mod(Pid, NewCheckMod) ->
  gen_server:cast(Pid, {change_check_mod, NewCheckMod}).

% @spec stop_checks(Pid :: pid()) -> ok
% @doc
%   If the _Pid_ is actively performing checks, this stops all future checks.
% @end
stop_checks(Pid) ->
  gen_server:cast(Pid, stop_checks).
