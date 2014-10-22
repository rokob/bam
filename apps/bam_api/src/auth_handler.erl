-module(auth_handler).

-export([init/3, rest_init/2]).
-export([allowed_methods/2, content_types_provided/2, content_types_accepted/2]).
-export([get_json/2, from_json/2]).

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  State = [],
  {ok, Req, State}.

allowed_methods(Req, State) ->
  Methods = [
    <<"GET">>,
    <<"PUT">>,
    <<"POST">>
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
  Result = <<"{\"hello\": \"world\"}">>,
  {Result, Req, State}.

from_json(Req, State) ->
  {true, Req, State}.
