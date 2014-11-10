-module(bam_auth_store).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0, create_user/3, new_token/3, verify_token/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SALT, <<"ALLYOURBaSeAREBeLONGTOus">>).
-record(state, {users}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% API

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_user(Username, Password, Key) ->
  gen_server:call(?SERVER, {create_user, Username, Password, Key}).

new_token(Username, Password, Key) ->
  gen_server:call(?SERVER, {token_request, Username, Password, Key}).

verify_token(Token, Key) ->
  gen_server:call(?SERVER, {verify_token, Token, Key}).

% Callbacks

init([]) ->
  {ok, initial_state()}.

handle_call({create_user, Username, Password, Key}, _From, State) ->
  case user_exists(Username, State) of
    true ->
      {reply, error, State};
    false ->
      NewState = store_username_password(Username, Password, State),
      JWT = make_JWT(Username, Key),
      Reply = {ok, JWT},
      {reply, Reply, NewState}
  end;
handle_call({token_request, Username, Password, Key}, _From, State) ->
  case check_username_password(Username, Password, State) of
    ok ->
      JWT = make_JWT(Username, Key),
      Reply = {ok, JWT},
      {reply, Reply, State};
    _ ->
      {reply, error, State}
  end;
handle_call({verify_token, Token, Key}, _From, State) ->
  try bam_jwt:decode(Token, Key) of
    error ->
      {reply, error, State};
    Payload ->
      Reply = {ok, Payload},
      {reply, Reply, State}
  catch
    error:_Reason ->
      {reply, error, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Private

initial_state() ->
  #state{users=orddict:new()}.

user_exists(Username, #state{users=Users}) ->
  orddict:is_key(Username, Users).

make_JWT(Username, Key) ->
  Payload = [{<<"username">>, Username}, {<<"expiration">>, get_expiration()}],
  bam_jwt:encode(Payload, Key).

check_username_password(Username, Password, #state{users=Users}) ->
  case orddict:find(Username, Users) of
    {ok, {PasswordHash, Pepper}} ->
      case do_hash(Password, Pepper) of
        PasswordHash -> ok;
        _            -> error
      end;
    error -> error
  end.

do_hash(V, Pepper) ->
  erlang:phash2(<< V/binary, $:, Pepper/binary, $:, ?SALT/binary >>).

store_username_password(Username, Password, State=#state{users=Users}) ->
  Pepper = crypto:strong_rand_bytes(8),
  PasswordHash = do_hash(Password, Pepper),
  NewUsers = orddict:store(Username, {PasswordHash, Pepper}, Users),
  NewState = State#state{users=NewUsers},
  NewState.

get_expiration() ->
  integer_to_binary(bam_lib:minutes_from_now(60)).

%% Test

-ifdef(TEST).
do_hash_test_() ->
  [
    {"it is deterministic",
      fun() ->
        Hash = do_hash(<<"hello there buddy">>, <<"P3ppr">>),
        Hash = do_hash(<<"hello there buddy">>, <<"P3ppr">>)
      end}
  ].

user_storing_test_() ->
  [
    {"when you store, it exists",
      fun() ->
        true = user_exists(<<"steve">>, store_username_password(<<"steve">>, <<"password">>, initial_state()))
      end},
    {"when you haven't stored it, it does not exist",
      fun() ->
        false = user_exists(<<"mike">>, store_username_password(<<"steve">>, <<"password">>, initial_state()))
      end}
  ].

check_username_password_test_() ->
  [
    {"when the password is correct, it is ok",
      fun() ->
        ok = check_username_password(<<"steve">>, <<"password">>,
          store_username_password(<<"steve">>, <<"password">>, initial_state()))
      end},
    {"when the password is wrong, it is an error",
      fun() ->
        error = check_username_password(<<"steve">>, <<"wrongpassword">>,
          store_username_password(<<"steve">>, <<"password">>, initial_state()))
      end},
    {"when the username doesn't exist, it is an error",
      fun() ->
        error = check_username_password(<<"mike">>, <<"password">>,
          store_username_password(<<"steve">>, <<"password">>, initial_state()))
      end}
  ].

make_JWT_test_() ->
  [
    {"the decoded payload contains the username",
      fun() ->
        Username = <<"bill">>,
        Key = <<"thisIsTheKey">>,
        Username = proplists:get_value(<<"username">>, bam_jwt:decode(make_JWT(Username, Key), Key))
      end}
  ].
-endif.
