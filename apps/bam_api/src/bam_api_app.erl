% @author rokob <wvvwwvw@gmail.com>
% @copyright rokob, 2014
% @doc
%   The application module for the Bam API
% @end

-module(bam_api_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(APPNAME, bam_api).

%% Application callbacks

start(_StartType, _StartArgs) ->
  cowboy:start_http(bam_api_listener, 100, [{port, 8080}],
    [{env,
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
  cowboy_router:compile([
    {<<"api.bam.io">>, routes()}
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
 catch _:_ -> false
 end.

valid_id(Value) when is_binary(Value) ->
  is_alphanumeric(Value).

%% TODO
is_alphanumeric(Value) when is_binary(Value) ->
  true.
