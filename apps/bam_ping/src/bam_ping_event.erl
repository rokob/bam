-module(bam_ping_event).

-export([start_link/0,
         add_handler/2, delete_handler/2,
         check/1]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
  gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
  gen_event:delete_handler(?SERVER, Handler, Args).

check(Data) ->
  gen_event:notify(?SERVER, {check, Data}).
