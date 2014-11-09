-module(bam_jwt).

-export([encode/2, encode/3, decode/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(HS256, <<"HS256">>).
-define(HS384, <<"HS384">>).
-define(HS512, <<"HS512">>).
-define(DEFAULT_ALGO, ?HS256).

encode(Payload, Key) ->
  encode(Payload, Key, ?DEFAULT_ALGO).

encode(Payload, KeyList, Algorithm) when is_list(KeyList) ->
  Key = base64url:decode(proplists:get_value(<<"k">>, KeyList)),
  encode(Payload, Key, Algorithm);
encode(Payload, Key, Algorithm) ->
  Header = {[{<<"typ">>, <<"JWT">>}, {<<"alg">>, Algorithm}]},
  HeaderJSON = jiffy:encode(Header),
  PayloadJSON = jiffy:encode({Payload}),
  encode(HeaderJSON, PayloadJSON, Key, Algorithm).

encode(HeaderJSON, PayloadJSON, Key, Algorithm) ->
  HeaderEncode = base64url:encode(HeaderJSON),
  PayloadEncode = base64url:encode(PayloadJSON),
  Data = <<HeaderEncode/binary, $., PayloadEncode/binary>>,
  Signature = base64url:encode(do_mac(Key, Data, Algorithm)),
  <<Data/binary, $., Signature/binary>>.

do_mac(Key, Data, ?HS256) ->
  crypto:hmac(sha256, Key, Data);
do_mac(Key, Data, ?HS384) ->
  crypto:hmac(sha384, Key, Data);
do_mac(Key, Data, ?HS512) ->
  crypto:hmac(sha512, Key, Data).

decode(JWT, KeyList) when is_list(KeyList) ->
  Key = base64url:decode(proplists:get_value(<<"k">>, KeyList)),
  decode(JWT, Key);
decode(JWT, Key) ->
  {Header, HeaderEncode, Rest} = decode_next_part(JWT),
  {Payload, PayloadEncode, SignatureEncode} = decode_next_part(Rest),
  Signature = base64url:decode(SignatureEncode),
  Data = <<HeaderEncode/binary, $., PayloadEncode/binary>>,
  Algorithm = proplists:get_value(<<"alg">>, Header),
  Check = do_mac(Key, Data, Algorithm),
  case Check of
    Signature -> Payload;
    _ -> error
  end.

decode_next_part(JWT) ->
  [NextEncode, Rest] = binary:split(JWT, <<$.>>),
  WrappedNext = jiffy:decode(base64url:decode(NextEncode)),
  case WrappedNext of
    {Next} -> {Next, NextEncode, Rest};
    _ -> {WrappedNext, NextEncode, Rest}
  end.

%% Test

-ifdef(TEST).
valid_version_test_() ->
  [
    {"http://self-issued.info/docs/draft-ietf-oauth-json-web-token.html Simple without whitespace",
      fun() ->
        Payload = [{<<"iss">>,<<"joe">>}, {<<"exp">>,1300819380},{<<"http://example.com/is_root">>,true}],
        Key = [{<<"k">>, <<"AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow">>}],
        JWT = <<"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLCJleHAiOjEzMDA4MTkzODAsImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.lliDzOlRAdGUCfCHCPx_uisb6ZfZ1LRQa0OJLeYTTpY">>,
        JWT = encode(Payload, Key, ?HS256)
      end},
    {"http://self-issued.info/docs/draft-ietf-oauth-json-web-token.html Simple",
      fun() ->
        HeaderJSON = [123, 34, 116, 121, 112, 34, 58, 34, 74, 87, 84, 34, 44, 13, 10, 32, 34, 97, 108, 103, 34, 58, 34, 72, 83, 50, 53, 54, 34, 125],
        PayloadJSON = [123, 34, 105, 115, 115, 34, 58, 34, 106, 111, 101, 34, 44, 13, 10, 32, 34, 101, 120, 112, 34, 58, 49, 51, 48, 48, 56, 49, 57,
                       51, 56, 48, 44, 13, 10, 32, 34, 104, 116, 116, 112, 58, 47, 47, 101, 120, 97, 109, 112, 108, 101, 46, 99, 111, 109, 47, 105,
                       115, 95, 114, 111, 111, 116, 34, 58, 116, 114, 117, 101, 125],
        Key = base64url:decode(<<"AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow">>),
        JWT = <<"eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk">>,
        JWT = encode(HeaderJSON, PayloadJSON, Key, ?HS256)
      end},
    {"decode(encode(X)) =:= X",
      fun() ->
        Payload = [{<<"iss">>,<<"joe">>}, {<<"exp">>,1300819380},{<<"http://example.com/is_root">>,true}],
        Key = <<"password">>,
        Payload = decode(encode(Payload, Key), Key)
      end},
    {"http://self-issued.info/docs/draft-ietf-oauth-json-web-token.html Simple decode",
      fun() ->
        Key = base64url:decode(<<"AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow">>),
        JWT = <<"eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk">>,
        Payload = [{<<"iss">>,<<"joe">>}, {<<"exp">>,1300819380},{<<"http://example.com/is_root">>,true}],
        Payload = decode(JWT, Key)
      end}
  ].
-endif.
