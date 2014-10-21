-module(bam_ping_worker).
-behaviour(gen_server).

-export([start_link/1,
         create/1, create/2, create/3,
         change_interval/2,
         change_check_type/2,
         stop_checks/1
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {host,
                check_interval,
                last_check,
                check_type}).

-define(DEFAULT_CHECK_INTERVAL, 60).
-define(DEFAULT_CHECK_TYPE, ping).

%% API

start_link(Configuration) ->
  gen_server:start_link(?MODULE, [Configuration], []).

create(Host) ->
  create(Host, ?DEFAULT_CHECK_INTERVAL).
create(Host, Interval) ->
  create(Host, Interval, ?DEFAULT_CHECK_TYPE).
create(Host, Interval, CheckType) ->
  bam_ping_worker_sup:start_child({Host, Interval, CheckType}).

change_interval(Pid, NewInterval) ->
  gen_server:cast(Pid, {change_interval, NewInterval}).

change_check_type(Pid, NewCheckType) ->
  gen_server:cast(Pid, {change_check_type, NewCheckType}).

stop_checks(Pid) ->
  gen_server:cast(Pid, stop_checks).

%% Callbacks

init([{Host, CheckInterval, CheckType}]) ->
  {ok, #state{host = Host,
              check_interval = CheckInterval,
              last_check = 0,
              check_type = CheckType}, 0}. 

handle_call(_Request, _From, State) ->
  {reply, ok, State, current_timeout(State)}.

handle_cast({change_interval, NewInterval}, State) ->
  NewState = State#state{check_interval = NewInterval},
  {noreply, NewState, current_timeout(NewState)};
handle_cast({change_check_type, NewCheckType}, State) ->
  NewState = State#state{check_type = NewCheckType},
  {noreply, NewState, current_timeout(NewState)};
handle_cast(stop_checks, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State, current_timeout(State)}.

handle_info(timeout, State) ->
  {ok, Time} = perform_check(State),
  NewState = State#state{last_check=Time},
  {noreply, NewState, current_timeout(NewState)};
handle_info(_Info, State) ->
  {noreply, State, current_timeout(State)}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Private

perform_check(#state{host=Host, check_type=CheckType}) ->
  CurrentTime = now_seconds(),
  io:format("[~p] Performing <~p> for Host: <~p>~n",
            [CurrentTime, CheckType, Host]),
  {ok, CurrentTime}.

now_seconds() ->
  Now = calendar:local_time(),
  calendar:datetime_to_gregorian_seconds(Now).

time_left(_CurrentTime, _LastCheck, 0) ->
  0;
time_left(CurrentTime, LastCheck, CheckInterval) ->
  TimeElapsed = CurrentTime - LastCheck,
  case CheckInterval - TimeElapsed of
      Time when Time =< 0 -> 0;
      Time                -> Time * 1000
  end.

current_timeout(State) ->
  CurrentTime = now_seconds(),
  current_timeout(CurrentTime, State).
current_timeout(CurrentTime, #state{last_check=LastCheck,
                                    check_interval=CheckInterval}) ->
  time_left(CurrentTime, LastCheck, CheckInterval).

%% Test

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

current_timeout_simple_test() ->
  State = #state{last_check = 42, check_interval=20},
  current_timeout(27, State) =:= time_left(27, 42, 20).

current_timeout_realtime_timeout_test() ->
  State = #state{last_check = 0, check_interval = 5},
  true = (current_timeout(State) =:= 0).

current_timeout_realtime_keep_going_test() ->
  State = #state{last_check = now_seconds()-1, check_interval = 60},
  true = (current_timeout(State) > 0).

time_left_test_() ->
  [
    {"Current and Last equal should have time left",
      fun() ->
        true = (time_left(42, 42, 20) > 0)
      end},
    {"Current more than interval from last should have no time left",
      fun() ->
        true = (time_left(40, 1, 10) =:= 0)
      end},
    {"Current different from Last but within interval",
      fun() ->
        true = (time_left(40, 35, 10) > 0)
      end}
  ].

-endif.
