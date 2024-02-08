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

%%% Includes.
-include("errors.hrl").

%%% Public API.
-export([show_errors/1, show_warnings/1, show_error/1, show_warning/1]).
-export([push_error/3, push_warning/3]).

%%% Types.
-export_type([fault/0, errors/0, warnings/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(S_ERROR, 'Error').
-define(S_WARNING, 'Warning').

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type severity() :: ?S_ERROR | ?S_WARNING.

-type fault() ::
  {Anno :: erl_anno:location() | none, Mod :: module(), Detail :: term()} |
  {Mod :: module(), Detail :: term()}.
-type errors() :: [fault()].
-type warnings() :: [fault()].


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

% I modelled the error display functions on the strange way EPP uses them to
% make everything consistent.
-spec show_errors([{file:filename(), errors()}]) -> any().
show_errors([]) ->
  ok;
% This is how the internal structure of the EPP works. When it has no errors, it returns [],
% otherwise it returns [{file,list of errors}].
show_errors([Error = {_, Errors}]) when is_list(Errors) ->
  show_faults(?S_ERROR, Error);
show_errors(Error = {_, Errors}) when is_list(Errors) ->
  show_faults(?S_ERROR, Error).

-spec show_warnings([{file:filename(), warnings()}]) -> any().
show_warnings([]) -> % This is how the internal structure of the EPP works. When it has no errors, it returns [],
% otherwise it returns [{file,list of errors}].
  ok;
show_warnings([Warning = {_, Warnings}]) when is_list(Warnings) ->
  show_faults(?S_WARNING, Warning);
show_warnings(Warning = {_, Warnings}) when is_list(Warnings) ->
  show_faults(?S_ERROR, Warning).

-spec show_error({file:name_all(), fault()} | fault()) -> ok.
show_error(E) ->
  show_fault(?S_ERROR, E).

-spec show_warning({file:name_all(), fault()} | fault()) -> ok.
show_warning(W) ->
  show_fault(?S_WARNING, W).


push_error(Mod, {Code, Node}, Error = #error{errors = Errors}) ->
%%  [{erl_syntax:get_pos(ErrNode), ?MODULE, {Class, ErrNode}} | Errors]
%%    {erl_syntax:get_pos(Data), ?MODULE, {Code, Data}}
%%    Error#error{errors = [{Code, Data} | Errors]}.
    Error#error{errors = [
      {erl_syntax:get_pos(Node), Mod, {Code, Node}} | Errors
    ]}.

push_warning(Mod, {Code, Node}, Error = #error{warnings = Warnings}) ->
  Error#error{warnings = [
    {erl_syntax:get_pos(Node), Mod, {Code, Node}} | Warnings
  ]}.


%%% ----------------------------------------------------------------------------
%%% Helper functions.
%%% ----------------------------------------------------------------------------

%% @private Prints the specified list of issues to standard_error.
-spec show_faults(string(), [{file:filename(), errors()}]) -> any().
show_faults(Level, {File, Errors}) ->
  File0 = lists:last(filename:split(File)),
  [show_fault(Level, {File0, E}) || E <- Errors].

%% @private Prints the specified issue to standard_error.
-spec show_fault(severity(), {file:name_all(), fault()} | fault()) -> ok.
show_fault(Severity, {File, {ANNO, Mod, E}}) ->
  io:format(
    standard_error, "~ts:~w: ~ts: ~ts~n",
    [File, ANNO, Severity, Mod:format_error(E)] % Delegate to the module showing the error.
  );
show_fault(Severity, {ANNO, Mod, E}) ->
  io:format(
    standard_error, "~w: ~ts: ~ts~n", [ANNO, Severity, Mod:format_error(E)] % Delegate to the module showing the error.
  );
show_fault(Severity, {Mod, E}) ->
  io:format(standard_error, "~ts: ~ts~n", [Severity, Mod:format_error(E)]). % Delegate to the module showing the error.