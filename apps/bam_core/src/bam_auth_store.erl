-module(bam_auth_store).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0, new_token/2, verify_token/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, #{tokens=[]}).

% API

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_token(Username, Password) ->
  gen_server:call(?SERVER, {token_request, Username, Password}).

verify_token(Username, Token, Signature) ->
  gen_server:call(?SERVER, {verify_token, Username, Token, Signature}).

% Callbacks

init([]) ->
  State = #state{},
  {ok, State}.

handle_call({token_request, Username, Password}, _From, State=#state{tokens=Tokens}) ->
  Token = <<"123">>,
  Hash = Password,
  Signature = [Username, Token],
  NewState = State#state{tokens=[{Username, {Hash, Token, Signature}} | Tokens]},
  Reply = [{token, Token}, {signature, Signature}],
  {reply, ok, NewState};
handle_call({verify_token, Username, Token, Signature}, _From, State=#state{tokens=Tokens}) ->
  case proplists:get_value(Username, Tokens) of
    {_, Token, Signature} ->
      {reply, ok, State};
    _ ->
      {reply, false, State}
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

