%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. May 2024 18:02
%%%-------------------------------------------------------------------
-module(pat_regex).
-author("duncan").

%% API
-export([]).
-compile(export_all).

%% TODO: Implement parser and the regular expression simplification functions.
%% TODO: To implement these functions just chuck them as part of the parser file as includes.

%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

%% @doc Checks whether a mailbox can become potentially empty.
%%
%% @param Regex commutative regular expression to check.
%%
%% @returns true if the mailbox can become potentially empty, false otherwise.
is_mb_empty(Regex) ->
%%  case re:run(Regex, "^(\\*.[A-Z][a-z]*|1\s*\\+.*|[A-Z][a-z]+1|[A-Z][a-z]+)$") of
  case re:run(trim(Regex), "^(\\*.*|1\\+.*|.*\\+1)$") of
    {match, _} ->
      true;
    _ ->
      false
  end.

% TODO: Requires a regular expression parser.
simplify([]) ->
  ok.

%% @private Removes all the spaces from the specified regular rexpression.
%%
%% @param Regex commutative regular expressions to trim.
%%
%% @returns trimmed regular expression.
trim(Regex) ->
  re:replace(Regex, "\s+", "", [global, {return, list}]).

