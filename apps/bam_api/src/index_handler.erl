-module(index_handler).

-export([init/3]).
-export([allowed_methods/2, content_types_provided/2]).
-export([get_json/2]).

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  Methods = [
    <<"OPTIONS">>,
    <<"HEAD">>,
    <<"GET">>
  ],
  {Methods, Req, State}.

content_types_provided(Req, State) ->
  JSON = {{<<"application">>, <<"json">>, '*'}, get_json},
  ContentTypes = [JSON],
  {ContentTypes, Req, State}.

get_json(Req, State) ->
  {AuthValue, Req2} = cowboy_req:meta(authorized, Req),
  Result = jiffy:encode({[{<<"hello">>, <<"world">>}, {<<"authorized">>, {AuthValue}}]}),
  {Result, Req2, State}.
