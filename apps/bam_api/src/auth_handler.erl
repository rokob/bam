-module(auth_handler).

-export([init/3, rest_init/2]).
-export([
  allowed_methods/2,
  content_types_accepted/2
  ]).
-export([
  from_json/2
  ]).

-define(KEY, <<122,117,80,198,194,198,125,154,134,248,105,58,140,57,212,129>>).
-define(SIGN, <<163,87,128,60,175,163,125,15,238,238,141,1>>).
-define(CHECK, <<"1123B5A9C686EAB942064CB352BA9A18">>).

-record(state, {key, sign}).

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  Key = bam_conf:get_val(bam_api, secrets, authkey, ?KEY),
  Sign = bam_conf:get_val(bam_api, secrets, signature, ?SIGN),
  State = #state{key=Key, sign=Sign},
  {ok, Req, State}.

allowed_methods(Req, State) ->
  Methods = [
    <<"PUT">>,
    <<"POST">>,
    <<"DELETE">>
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
      handle_login(Req2, State);
    <<"PUT">> ->
      handle_update(Req2, State);
    _ ->
      {false, Req2, State}
  end.

handle_login(Req, State#state{key=Key, sign=SignKey}) ->
  {JSON, Req2} = get_json_body(Req),
  {ok, Username, Password} = get_user_data_from_json(JSON),
  case validate_user(Username, Password, Key) of
    true ->
      Token = create_token(),
      Signature = sign_token(Username, Token, SignKey),
      Response = jiffy:encode({[{<<"status">>, <<"new">>}, {<<"token">>, Token}, {<<"signature">>, Signature}]}),
      Req3 = cowboy_req:set_resp_body(Response, Req2),
      {true, Req3, State};
    _ ->
      Response = jiffy:encode({[{<<"error">>, <<"Unauthorized">>}]}),
      Req3 = cowboy_req:set_resp_body(Response, Req2),
      {false, Req3, State}
  end.

handle_update(Req, State) ->
  {JSON, Req2} = get_json_body(Req),
  {ok, Username, Token, Signature} = get_update_data_from_json(JSON),
  case validate_update_data(Username, Token, Signature) of
    true ->
      NewToken = create_token(),
      NewSignature = sign_token(Username, NewToken, SignKey),
      Response = jiffy:encode({[{<<"status">>, <<"refreshed">>}, {<<"token">>, NewToken}, {<<"signature">>, NewSignature}]}),
      Req3 = cowboy_req:set_resp_body(Response, Req2),
      {true, Req3, State};
    _ ->
      Response = jiffy:encode({[{<<"error">>, <<"Unauthorized">>}]}),
      Req3 = cowboy_req:set_resp_body(Response, Req2),
      {false, Req3, State}
  end.

get_json_body(Req) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  {jiffy:decode(Body), Req2}.

create_token() ->
  bam_lib:bin_to_hex(crypto:strong_rand_bytes(16)).

sign_token(Username, Token, SignKey) ->
  do_hmac(SignKey, [Username, Token]).

get_user_data_from_json({[]}) -> false;
get_user_data_from_json({Proplist}) ->
  Username = proplists:get_value(<<"username">>, Proplist),
  Password = proplists:get_value(<<"password">>, Proplist),
  {ok, Username, Password}.

get_update_data_from_json({[]}) -> false;
get_update_data_from_json({Proplist}) ->
  Username = proplists:get_value(<<"username">>, Proplist),
  Token = proplists:get_value(<<"token">>, Proplist),
  Signature = proplists:get_value(<<"signature">>, Proplist),
  {ok, Username, Token, Signature}.

validate_user(Username, Password, Key) ->
  ?CHECK =:= do_hmac(Key, [Username, Password]).

validate_update_data(Username, Token, Signature) ->
  Signature =:= sign_token(Username, Token).

do_hmac(Key, Msg) ->
  bam_lib:bin_to_hex(crypto:hmac(sha256, Key, Msg, 16)).
