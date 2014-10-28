% @author rokob <wvvwwvw@gmail.com>
% @copyright rokob, 2014
% @doc
%   The application module for the Bam API
% @end

-module(bam_api_app).
-behaviour(application).

-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(APPNAME, bam_api).

%% Application callbacks

start(_StartType, _StartArgs) ->
  Port = bam_conf:get_val(bam_api, api, port, 8080),
  cowboy:start_http(bam_api_listener, 100, [{port, Port}],
    [{middlewares, [cowboy_router, auth_middleware, cowboy_handler]},
     {env,
      [
        {dispatch, dispatch()}
      ]
    }]
  ),
  bam_api_sup:start_link().

stop(_State) ->
  ok.

%% Private

dispatch() ->
  Host = bam_conf:get_val(bam_api, api, host, <<"api.bambam.io">>),
  cowboy_router:compile([
    {Host, routes()}
  ]).

routes() ->
  VersionConstraint = {version, function, fun valid_version/1},
  IdConstraint = {id, function, fun valid_id/1},
  [
    {"/assets/[...]", cowboy_static, {priv_dir, ?APPNAME, "static/assets"}},
    {"/:version", [VersionConstraint], index_handler, []},
    {"/:version/auth", [VersionConstraint], auth_handler, []},
    {"/:version/service", [VersionConstraint], service_handler, []},
    {"/:version/service/:id", [VersionConstraint, IdConstraint], service_handler, []}
  ].

valid_version(Value) when is_binary(Value) ->
 try
   [$v | Num] = binary_to_list(Value),
   {true, list_to_integer(Num)}
 catch
   _:_ -> false
 end.

valid_id(Value) when is_binary(Value) ->
  lists:all(fun valid_character_pred/1, binary_to_list(Value)).

valid_character_pred(C) ->
  (C > 47 andalso C < 58) orelse (C > 64 andalso C < 91) orelse (C > 96 andalso C < 123).

%% Test

-ifdef(TEST).
valid_version_test_() ->
  [
    {"Completely wrong is not a version",
      fun() ->
        false = valid_version(<<"blah">>)
      end},
    {"Correct version is parsed to a number",
      fun() ->
        {true, 42} = valid_version(<<"v42">>)
      end},
    {"Partially right is still wrong",
      fun() ->
        false = valid_version(<<"v0x">>)
      end}
  ].

valid_id_test_() ->
  [
    {"All alphanumeric is acceptable",
      fun() ->
        true = valid_id(<<"aBcD123">>)
      end},
    {"Random characters are not allowed",
      fun() ->
        false = valid_id(<<"abc$$$">>)
      end}
  ].
-endif.
