%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. May 2023 15:09
%%%-------------------------------------------------------------------
-module(errors).
-author("duncan").

%%% Public API.
-export([show_errors/1, show_warnings/1, show_error/1, show_warning/1]).

%%% Types.
-export_type([error/0, errors/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(L_ERROR, "Error").
-define(L_WARNING, "Warning").

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type error() ::
{Anno :: erl_anno:location() | none, Mod :: module(), Detail :: term()} |
{Mod :: module(), Detail :: term()}.
-type errors() :: [error()].


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-spec show_errors([{file:filename(), errors()}]) -> any().
show_errors(Errors = [{_, _}]) ->
  show_errors(?L_ERROR, Errors).

-spec show_warnings([{file:filename(), errors()}]) -> any().
show_warnings(Warnings = [{_, _}]) ->
  show_errors(?L_WARNING, Warnings).

-spec show_error({file:name_all(), error()} | error()) -> ok.
show_error(E) ->
  show_error(?L_ERROR, E).

-spec show_warning({file:name_all(), error()} | error()) -> ok.
show_warning(W) ->
  show_error(?L_WARNING, W).


%%% ----------------------------------------------------------------------------
%%% Helper functions.
%%% ----------------------------------------------------------------------------

%% @private Prints the specified list of issues to standard_error.
-spec show_errors(string(), [{file:filename(), errors()}]) -> any().
show_errors(Level, [{File, Errors}]) ->
  File0 = lists:last(filename:split(File)),
  [show_error(Level, {File0, E}) || E <- Errors].

%% @private Prints the specified issue to standard_error.
-spec show_error(string(), {file:name_all(), error()} | error()) -> ok.
show_error(Severity, {File, {ANNO, Mod, E}}) ->
  io:format(
    standard_error, "~ts:~w: ~ts: ~ts~n",
    [File, ANNO, Severity, Mod:format_error(E)]
  );
show_error(Severity, {ANNO, Mod, E}) ->
  io:format(
    standard_error, "~w: ~ts: ~ts~n", [ANNO, Severity, Mod:format_error(E)]
  );
show_error(Severity, {Mod, E}) ->
  io:format(standard_error, "~ts: ~ts~n", [Severity, Mod:format_error(E)]).