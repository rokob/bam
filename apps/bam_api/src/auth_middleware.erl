-module(auth_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

-define(AUTH_HEADER, <<"x-bam-auth-token">>).

execute(Req, Env) ->
  {authkey, Key} = lists:keyfind(authkey, 1, Env),
  case is_auth_handler(Req) of
    {true, Req2} ->
      Req3 = cowboy_req:set_meta(authorized, auth, Req2),
      {ok, Req3, Env};
    {_, Req2} ->
      case is_authorized(Key, Req2) of
        {authorized, Value} ->
          Req3 = cowboy_req:set_meta(authorized, Value, Req2),
          {ok, Req3, Env};
        {error, StatusCode} ->
          {error, StatusCode, Req2}
      end
  end.

is_auth_handler(Req) ->
  {Path, Req2} = cowboy_req:path(Req),
  try split_second(split_second(Path)) of
    <<"auth">> ->
      {true, Req2};
    _ ->
      {false, Req2}
  catch
    error:_Reason ->
      {false, Req2}
  end.

split_second(Binary) ->
  [_, Result] = binary:split(Binary, <<$/>>),
  Result.

is_authorized(Key, Req) ->
  case check_header(Req) of
    false  ->
      {error, 401};
    debug ->
      {authorized, <<"DEBUG">>};
    Value ->
      case bam_auth_store:verify_token(Value, Key) of
        {ok, Payload} ->
          {authorized, Payload};
        _ ->
          {error, 401}
      end
  end.

check_header(Req) ->
  case cowboy_req:header(?AUTH_HEADER, Req, undefined) of
    {undefined, _Req2} ->
      false;
    {<<"DEBUG">>, _Req2} ->
      debug;
    {Value, _Req2} ->
      Value
  end.
