-module(auth_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

-define(AUTH_HEADER, <<"x-bam-auth-token">>).

execute(Req, Env) ->
  case is_authorized(Req) of
    {authorized, Value} ->
      Req2 = cowboy_req:set_meta(authorized, Value, Req),
      {ok, Req2, Env};
    {error, StatusCode} ->
      {error, StatusCode, Req}
  end.

is_authorized(Req) ->
  case check_header(Req) of
    false  ->
      {error, 401};
    Value ->
      {authorized, Value}
  end.

check_header(Req) ->
  case cowboy_req:header(?AUTH_HEADER, Req, undefined) of
    {undefined, _Req2} ->
      false;
    {Value, _Req2} ->
      Value
  end.