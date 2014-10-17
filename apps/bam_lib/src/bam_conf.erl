-module(bam_conf).

-export([get_section/1, get_section/2, get_section/3]).
-export([get_val/2, get_val/3, get_val/4]).

%% API Functions

% @spec get_section(atom()) -> [{atom(), any()}] | undefined
% @doc Get a section of from the configuration data, returning undefined if
% it is not found.
% @end
get_section(Name) ->
  get_section(Name, undefined).

get_section(Name, Default) ->
  case application:get_env(Name) of
    {ok, V} ->
      V;
    _ ->
      Default
  end.

get_section(Application, Name, Default) ->
  case application:get_env(Application, Name) of
    {ok, V} ->
      V;
    _ ->
      Default
  end.

get_val(SectionName, Name) ->
  get_val(SectionName, Name, undefined).

get_val(SectionName, Name, Default) ->
  get_val_from_section(application:get_env(SectionName), Name, Default).

get_val(Application, SectionName, Name, Default) ->
  get_val_from_section(application:get_env(Application, SectionName), Name, Default).

get_val_from_section({ok, Section}, Name, Default) ->
  proplists:get_value(Name, Section, Default);
get_val_from_section(_, _, Default) ->
  Default.
