%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jun 2024 17:30
%%%-------------------------------------------------------------------
-module(paterl_tools).
-author("duncan").

%% API
-export([fresh_var/1]).

%%-compile(export_all).

-define(VAR__ID_START, 0).

-define(VAR__ID, '@var_id').

%%-doc "Returns a fresh variable name".
fresh_var(Name) when is_atom(Name) ->
  list_to_atom(
    string:to_lower(atom_to_list(Name)) ++ integer_to_list(next_var_id(Name))
  ).

%%-doc "Returns the next ID for the specified name".
%%-doc hidden.
next_var_id(Name) when is_atom(Name) ->
  Key = {?VAR__ID, Name},
  case get(Key) of
    undefined ->
      put(Key, ?VAR__ID_START + 1),
      ?VAR__ID_START;
    Id ->
      put(Key, Id + 1)
  end.

