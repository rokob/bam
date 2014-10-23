% @author rokob <wvvwwvw@gmail.com>
% @copyright rokob, 2014
% @doc
%   The Bam Ping API
% @end

-module(bam_ping).

-export([create/2, create/3, create/4, create/5,
         change_interval/2, change_check_mod/2,
         stop_checks/1]).

-define(DEFAULT_CHECK_INTERVAL, 60).
-define(DEFAULT_CHECK_MOD, bam_ping_check_ping).

% @spec create(Host :: string(), Port :: integer()) -> {ok, pid()}
% @doc
%   Create a new worker and start it pinging the 'Host' on 'Port'
%   using the default time interval of '60' seconds and the default
%   check type (http ping).
% @end
create(Host, Port) ->
  create(Host, Port, ?DEFAULT_CHECK_INTERVAL).

% @spec create(Host :: string(), Port :: integer(), Interval :: integer()) -> {ok, pid()}
% @doc
%   Create a new worker and start it pinging the 'Host' on 'Port'
%   every 'Interval' seconds using the default
%   check type (http ping).
% @end
create(Host, Port, Interval) ->
  create(Host, Port, Interval, ?DEFAULT_CHECK_MOD).

% @spec create(Host :: string(),
%              Port :: integer(),
%              Interval :: integer(),
%              CheckMod :: atom()) -> {ok, pid()}
% @doc
%   Create a new worker and start it pinging the 'Host' on 'Port'
%   every 'Interval' seconds using the check defined by 'CheckMod'.
%   An empty list will be passed as the options parameter to 'CheckMod:init/3'.
% @end
create(Host, Port, Interval, CheckMod) ->
  create(Host, Port, Interval, CheckMod, []).

% @spec create(Host :: string(),
%              Port :: integer(),
%              Interval :: integer(),
%              CheckMod :: atom(),
%              CheckOpts :: term()) -> {ok, pid()}
% @doc
%   Create a new worker and start it pinging the 'Host' on 'Port'
%   every 'Interval' seconds using the check defined by 'CheckMod'.
%   The 'CheckOpts' will be passed to 'CheckMod:init/3'.
% @end
create(Host, Port, Interval, CheckMod, CheckOpts) ->
  bam_ping_worker:create(Host, Port, Interval, CheckMod, CheckOpts).

% @spec change_interval(Pid :: pid(), NewInterval :: integer()) -> ok
% @doc
%   Make the checks occur every 'NewInterval' seconds.
% @end
change_interval(Pid, NewInterval) ->
  gen_server:cast(Pid, {change_interval, NewInterval}).

% @spec change_check_mod(Pid :: pid(), NewCheckMod :: atom()) -> ok
% @doc
%   Make the checks use 'NewCheckMod' to define the type of check performed.
%   Passes an empty list as the options to 'CheckMod:init/3'.
% @end
change_check_mod(Pid, NewCheckMod) ->
  change_check_mod(Pid, NewCheckMod, []).

% @spec change_check_mod(Pid :: pid(), NewCheckMod :: atom(), CheckOpts :: term()) -> ok
% @doc
%   Make the checks use 'NewCheckMod' to define the type of check performed.
%   Passes 'CheckOpts' as the options to 'CheckMod:init/3'.
% @end
change_check_mod(Pid, NewCheckMod, CheckOpts) ->
  gen_server:cast(Pid, {change_check_mod, NewCheckMod, CheckOpts}).

% @spec stop_checks(Pid :: pid()) -> ok
% @doc
%   If the 'Pid' is actively performing checks, this stops all future checks.
% @end
stop_checks(Pid) ->
  gen_server:cast(Pid, stop_checks).
