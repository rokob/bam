-module(auth_handler).

-export([init/3, rest_init/2]).
-export([
  allowed_methods/2,
  content_types_accepted/2
  ]).
-export([
  from_json/2
  ]).

-record(state, {key}).

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, [Key]) ->
  State = #state{key=Key},
  {ok, Req, State}.

allowed_methods(Req, State) ->
  Methods = [
    <<"PUT">>,
    <<"POST">>
  ],
  {Methods, Req, State}.

content_types_accepted(Req, State) ->
  JSON = {{<<"application">>, <<"json">>, []}, from_json},
  ContentTypes = [JSON],
  {ContentTypes, Req, State}.

from_json(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  case Method of
    <<"POST">> ->
      handle_auth(create_user, Req2, State);
    <<"PUT">> ->
      handle_auth(new_token, Req2, State);
    _ ->
      {false, Req2, State}
  end.

handle_auth(Method, Req, #state{key=Key}=State) ->
  {JSON, Req2} = get_json_body(Req),
  {ok, Username, Password} = get_user_data_from_json(JSON),
  AuthResponse = bam_auth_store:Method(Username, Password, Key),
  token_reply(AuthResponse, Req2, State).

token_reply({ok, Token}, Req, State) ->
  Response = jiffy:encode({[{<<"status">>, <<"success">>}, {<<"token">>, Token}]}),
  Req2 = cowboy_req:set_resp_body(Response, Req),
  {true, Req2, State};
token_reply(_, Req, State) ->
  Response = jiffy:encode({[{<<"status">>, <<"failure">>}, {<<"message">>, <<"Invalid username/password">>}]}),
  Req2 = cowboy_req:set_resp_body(Response, Req),
  {false, Req2, State}.

get_json_body(Req) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  {jiffy:decode(Body), Req2}.

get_user_data_from_json({[]}) -> false;
get_user_data_from_json({Proplist}) ->
  Username = proplists:get_value(<<"username">>, Proplist),
  Password = proplists:get_value(<<"password">>, Proplist),
  {ok, Username, Password}.
