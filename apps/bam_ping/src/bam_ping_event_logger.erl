-module(bam_ping_event_logger).
-behaviour(gen_event).

-export([add_handler/0, delete_handler/0]).

-export([init/1,
         handle_event/2, handle_call/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {}).

add_handler() ->
  bam_ping_event:add_handler(?MODULE, []).

delete_handler() ->
  bam_ping_event:delete_handler(?MODULE, []).

init([]) ->
  {ok, #state{}}.

handle_event({check, Data}, State) ->
  io:format("~nCheck Performed :: ~p~n", [Data]),
  {ok, State};
handle_event(_Event, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
