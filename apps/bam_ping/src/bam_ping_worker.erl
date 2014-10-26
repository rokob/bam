-module(bam_ping_worker).
-behaviour(gen_server).

-export([start_link/5, create/5]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {host,
                port,
                last_check,
                check}).

-record(check, {mod, state, interval}).

%% API

start_link(Host, Port, Interval, CheckMod, CheckOpts) ->
  gen_server:start_link(?MODULE, [Host, Port, Interval, CheckMod, CheckOpts], []).

create(Host, Port, Interval, CheckMod, CheckOpts) ->
  bam_ping_worker_sup:start_child(Host, Port, Interval, CheckMod, CheckOpts).

%% Callbacks

init([Host, Port, CheckInterval, CheckMod, CheckOpts]) ->
  case CheckMod:init(Host, Port, CheckOpts) of
    {ok, CheckState} ->
      State = construct_state(Host, Port, CheckMod, CheckState, CheckInterval),
      {ok, State, 0};
    {stop, Reason} ->
      {stop, Reason}
  end.

handle_call(_Request, _From, State) ->
  {reply, ok, State, current_timeout(State)}.

handle_cast({change_interval, NewInterval}, State) ->
  NewState = update_check_interval(State, NewInterval),
  {noreply, NewState, current_timeout(NewState)};
handle_cast({change_check_mod, NewCheckMod, CheckOpts}, State) ->
  case NewCheckMod:init(State#state.host, State#state.port, CheckOpts) of
    {ok, NewCheckState} ->
      NewState = update_check_mod_and_state(State, NewCheckMod, NewCheckState),
      {noreply, NewState, current_timeout(NewState)};
    {stop, Reason} ->
      {stop, Reason, State}
  end;
handle_cast(stop_checks, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State, current_timeout(State)}.

handle_info(timeout, State) ->
  CurrentTime = now_seconds(),
  case perform_check(State) of
    {Time, {ok, Result, NewModState}} ->
      bam_ping_event:check([{result, Result}, {time, Time}]),
      NewState = update_check_state_and_time(State, NewModState, CurrentTime),
      {noreply, NewState, current_timeout(NewState)};
    {_Time, {stop, Reason, NewModState}} ->
      {stop, Reason, update_check_state_and_time(State, NewModState, CurrentTime)}
  end;
handle_info(_Info, State) ->
  {noreply, State, current_timeout(State)}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Private

construct_state(Host, Port, CheckMod, CheckState, CheckInterval) ->
  Check = #check{mod = CheckMod,
                  state = CheckState,
                  interval = CheckInterval
                },
  #state{host = Host,
         port = Port,
         last_check = 0,
         check = Check}.

update_check_state_and_time(State, NewCheckState, CurrentTime) ->
  NewCheck = State#state.check#check{state=NewCheckState},
  State#state{last_check=CurrentTime, check=NewCheck}.

update_check_mod_and_state(State, NewCheckMod, NewCheckState) ->
  State#state.check#check{state = NewCheckState, mod = NewCheckMod}.

update_check_interval(State, NewInterval) ->
  State#state.check#check{interval = NewInterval}.

now_seconds() ->
  Now = calendar:local_time(),
  calendar:datetime_to_gregorian_seconds(Now).

current_timeout(State) ->
  CurrentTime = now_seconds(),
  current_timeout(CurrentTime, State).
current_timeout(CurrentTime, #state{last_check=LastCheck,
                                    check=#check{interval=CheckInterval}}) ->
  time_left(CurrentTime, LastCheck, CheckInterval).

time_left(_CurrentTime, _LastCheck, 0) ->
  0;
time_left(CurrentTime, LastCheck, CheckInterval) ->
  TimeElapsed = CurrentTime - LastCheck,
  case CheckInterval - TimeElapsed of
      Time when Time =< 0 -> 0;
      Time                -> Time * 1000
  end.

perform_check(#state{host=Host, port=Port, check=#check{mod=Mod, state=State}}) ->
  timer:tc(Mod, perform, [Host, Port, State]).

%% Test

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

current_timeout_simple_test() ->
  State = #state{last_check = 42, check=#check{interval=20}},
  current_timeout(27, State) =:= time_left(27, 42, 20).

current_timeout_realtime_timeout_test() ->
  State = #state{last_check = 0, check=#check{interval = 5}},
  true = (current_timeout(State) =:= 0).

current_timeout_realtime_keep_going_test() ->
  State = #state{last_check = now_seconds()-1, check=#check{interval = 60}},
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
      end},
    {"An interval of 0 always has no time left",
      fun() ->
        true = (time_left(10, 200, 0) =:= 0)
      end},
    {"An interval of 0 always has no time left seriously",
      fun() ->
        true = (time_left(200, 10, 0) =:= 0)
      end}
  ].

-endif.
