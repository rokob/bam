-module(bam_ping_check).

-callback init(Host :: binary(), Port :: integer(), Opts :: term()) ->
  {ok, State :: term()} |
  {stop, Reason :: term()}.

-callback perform(Host :: binary(), Port :: integer(), State :: term()) ->
  {ok, Result :: term(), NewState :: term()} |
  {stop, Reason :: term(), NewState :: term()}.
