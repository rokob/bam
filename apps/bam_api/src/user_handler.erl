-module(user_handler).

-export([init/3, rest_init/2]).
-export([
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2
  ]).
-export([
  get_json/2,
  from_json/2
  ]).

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, []}.

allowed_methods(Req, State) ->
  Methods = [
    <<"GET">>,
    <<"PUT">>,
    <<"DELETE">>
  ],
  {Methods, Req, State}.

content_types_provided(Req, State) ->
  JSON = {{<<"application">>, <<"json">>, '*'}, get_json},
  ContentTypes = [JSON],
  {ContentTypes, Req, State}.

content_types_accepted(Req, State) ->
  JSON = {{<<"application">>, <<"json">>, []}, from_json},
  ContentTypes = [JSON],
  {ContentTypes, Req, State}.

get_json(Req, State) ->
  handle_show_user(Req, State).

from_json(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  case Method of
    <<"PUT">> ->
      handle_update_user(Req2, State);
    <<"DELETE">> ->
      handle_delete_user(Req2, State);
    _ ->
      {false, Req2, State}
  end.

handle_show_user(Req, State) ->
  {<<"{\"hey\":42}">>, Req, State}.

handle_update_user(Req, State) ->
  Req2 = cowboy_req:set_resp_body(<<"{\"update_user\":42}">>, Req),
  {true, Req2, State}.

handle_delete_user(Req, State) ->
  Req2 = cowboy_req:set_resp_body(<<"{\"delete_user\":42}">>, Req),
  {true, Req2, State}.

get_json_body(Req) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  {jiffy:decode(Body), Req2}.
