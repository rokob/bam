-module(bam_set).

% This doesn't work yet

-export([new/0, make_set/2, union/3, find_set/2]).

new() -> [].

make_set(Value, S) ->
  [{Value, {self, 0, Value}} | S].

union(X, Y, S) ->
  A = proplists:get_value(X, S),
  B = proplists:get_value(Y, S),
  {Aroot, S1} = find_set(A, S),
  {Broot, S2} = find_set(B, S1),
  link(Aroot, Broot, S2).

find_set(undefined, _) -> undefined;
find_set(Me={self, _, _}, S) ->
  {Me, S};
find_set({Parent, Rank, Value}, S) ->
  {Root, S2} = find_set(Parent, S),
  {Root, replace(Value, {Root, Rank, Value}, S2)}.

link(A={_, Arank, _}, {_, Brank, Bvalue}, S) when Arank > Brank ->
  replace(Bvalue, {A, Brank, Bvalue}, S);
link({_, Arank, Avalue}, {Bp, Brank, Bvalue}, S) when Arank =:= Brank ->
  replace(Avalue, {{Bp, Brank+1, Bvalue}, Arank, Avalue}, S);
link({_, Arank, Avalue}, B, S) ->
  replace(Avalue, {B, Arank, Avalue}, S).

replace(Key, Value, List) ->
  lists:keyreplace(Key, 1, List, Value).
