-module(bam_auth_store).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0, new_token/3, verify_token/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SALT, <<"ALLYOURBaSeAREBeLONGTOus">>).
-record(state, {users}).

% API

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_token(Username, Password, Key) ->
  gen_server:call(?SERVER, {token_request, Username, Password, Key}).

verify_token(Token, Key) ->
  gen_server:call(?SERVER, {verify_token, Token, Key}).

% Callbacks

init([]) ->
  State = #state{users=orddict:new()},
  {ok, State}.

handle_call({token_request, Username, Password, Key}, _From, State) ->
  case check_username_password(Username, Password, State) of
    ok ->
      Payload = [{username, Username}, {expiration, get_expiration()}],
      JWT = bam_jwt:encode(Payload, Key),
      Reply = {ok, JWT},
      {reply, Reply, State};
    _ ->
      {reply, error, State}
  end;
handle_call({verify_token, Token, Key}, _From, State) ->
  case bam_jwt:decode(Token, Key) of
    error ->
      {reply, error, State};
    Payload ->
      Reply = {ok, Payload},
      {reply, Reply, State}
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
