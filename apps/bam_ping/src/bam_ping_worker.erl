-module(bam_ping_worker).
-behaviour(gen_server).

-export([start_link/4, create/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {host,
                port,
                last_check,
                check}).

-record(check, {mod, state, interval}).

%% API

start_link(Host, Port, Interval, CheckMod) ->
  gen_server:start_link(?MODULE, [Host, Port, Interval, CheckMod], []).

create(Host, Port, Interval, CheckMod) ->
  bam_ping_worker_sup:start_child(Host, Port, Interval, CheckMod).

%% Callbacks

init([Host, Port, CheckInterval, CheckMod]) ->
  case CheckMod:init(Host, Port) of
    {ok, State} ->
      Check = #check{mod = CheckMod,
                     state = State,
                     interval = CheckInterval
                    },
      {ok, #state{host = Host,
                  port = Port,
                  last_check = 0,
                  check = Check}, 0};
    {stop, Reason} ->
      {stop, Reason}
  end.

handle_call(_Request, _From, State) ->
  {reply, ok, State, current_timeout(State)}.

handle_cast({change_interval, NewInterval}, State) ->
  NewState = State#state.check#check{interval = NewInterval},
  {noreply, NewState, current_timeout(NewState)};
handle_cast({change_check_mod, NewCheckMod}, State) ->
  case NewCheckMod:init(State#state.host, State#state.port) of
    {ok, NewCheckState} ->
      NewState = State#state.check#check{state = NewCheckState, mod = NewCheckMod},
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
      io:format("Got ~p, Took ~p ms~n", [Result, Time]),
      NewState = State#state{last_check=CurrentTime,
                             check=State#state.check#check{state=NewModState}},
      {noreply, NewState, current_timeout(NewState)};
    {_Time, {stop, Reason, NewModState}} ->
      NewState = State#state{last_check=CurrentTime,
                             check=State#state.check#check{state=NewModState}},
      {stop, Reason, NewState}
  end;
handle_info(_Info, State) ->
  {noreply, State, current_timeout(State)}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Private

perform_check(#state{host=Host, port=Port, check=#check{mod=Mod, state=State}}) ->
  timer:tc(Mod, perform, [Host, Port, State]).

% now_milliseconds() ->
%   {Mega, Sec, Micro} = os:timestamp(),
%   (Mega*1000000 + Sec)*1000 + round(Micro/1000).

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
                                    check=#check{interval=CheckInterval}}) ->
  time_left(CurrentTime, LastCheck, CheckInterval).

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
