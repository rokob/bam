-module(auth_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
  case is_authorized(Req) of
    {authorized, Value} ->
      {ok, Req, [{authorized, Value} | Env]};
    {error, StatusCode} ->
      {error, StatusCode, Req}
  end.

is_authorized(Req) ->
  case check_header(Req) of
    ok ->
      {authorized, true};
    _  ->
      {error, 401}
  end.

check_header(_Req) ->
  ok.
