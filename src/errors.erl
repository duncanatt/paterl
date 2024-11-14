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
-export([new/0, new/2]).
-export([show_errors/1, show_warnings/1, show_error/1, show_warning/1]).
-export([push_error/3, push_warning/3]).

%%% Public types.
%%-export_type([error/0, reason/0, errors/0, warnings/0]).
-export_type([error/0, reason/0, source/0, reasons/0]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(S_ERROR, 'Error').
-define(S_WARNING, 'Warning').

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type severity() :: ?S_ERROR | ?S_WARNING.

-type detail() :: term().

-type reason() ::
  {Anno :: erl_anno:location() | none, Mod :: module(), Detail :: detail()} |
  {Mod :: module(), Detail :: detail()}.

-type reasons() :: [reason()].

%%-type errors() :: reasons().
%%-type warnings() :: reasons().

-type error() :: #error{}.

% This is how the internal structure of the EPP works. When it has no errors, it returns [],
% otherwise it returns [{file,list of errors}].
% I modelled the error display functions on the strange way EPP uses them to
% make everything consistent.
%% The source file containing errors.
%%-type source() ::
%%  [{Filename :: file:filename(), errors() | warnings()}] |
%%  {Filename :: file:filename(), errors() | warnings()}.

-type source() ::
  [{Filename :: file:filename(), reasons()}] | % Matches with erl_lint errors and warnings.
  {Filename :: file:filename(), reasons()}. % Matches with paterl errors and warnings.

%%-type errors() :: source().
%%-type warnings() :: source().

%%-type error() ::

%%-type error() :: {file:name_all(), reason()} | reason().


%%Warnings :: [{SourceFile,[ErrorInfo]}],
%%Errors :: [{SourceFile,[ErrorInfo]}],
%%-type error_info() :: {erl_anno:location()|'none', module(), error_description()}.

%%Source = [
%%  {Filename = "./src/examples/erlang/codebeam/id_server_demo.erl",
%%  Reasons = [Reason = {Anno = 43, Mod = erl_lint, Detail = {unused_type,{main_mb,0}}}]}
%%]
%%
%%Source = [
%%  {
%%  Filename = "./src/examples/erlang/codebeam/id_server_demo.erl",
%%  Reasons = [
%%    Reason = {43,erl_lint,{unused_type,{main_mb,0}}},
%%    Reason = {44,erl_lint,{unused_type,{main_mb2,0}}}]
%%  }
%%]

%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------


new() ->
  #error{}.

new(Errors, Warnings) when is_list(Errors), is_list(Warnings) ->
  #error{errors = Errors, warnings = Warnings}.



%%-spec show_errors([{file:filename(), errors()}]) -> any().
-spec show_errors(source()) -> any().
%%show_errors([]) -> %TODO: Test whether this case is taken.
%%  ok;
show_errors([Errors = {_Filename, Reasons}]) when is_list(Reasons) ->
  % Handles erl_lint errors.
  print_reasons(?S_ERROR, Errors);
show_errors(Errors = {_Filename, Reasons}) when is_list(Reasons) ->
  % Handles paterl errors.
  print_reasons(?S_ERROR, Errors).

-spec show_warnings(source()) -> any().
%%show_warnings([]) -> %TODO: Test whether this case is taken.
%%  ok;
%%show_warnings([Warnings = {_Filename, Reasons}]) when is_list(Reasons) ->
%%  % Handles erl_lint warnings.
%%  print_reasons(?S_WARNING, Warnings);
show_warnings(Warnings = {_Filename, Reasons}) when is_list(Reasons) ->
  % Handles paterl warnings.
  print_reasons(?S_WARNING, Warnings).



%%-spec show_error({file:name_all(), reason()} | reason()) -> ok.
-spec show_error(reason()) -> ok.
show_error(Reason) ->
  print_reason(?S_ERROR, Reason).

%%-spec show_warning({file:name_all(), reason()} | reason()) -> ok.
-spec show_warning(reason()) -> ok.
show_warning(Reason) ->
  print_reason(?S_WARNING, Reason).
%%  {Base, {ANNO, Mod, Detail}} or no base.

-spec push_error(module(), detail(), error()) -> error().
push_error(Mod, {Code, Node}, Error = #error{errors = Reasons}) ->
%%  [{erl_syntax:get_pos(ErrNode), ?MODULE, {Class, ErrNode}} | Errors]
%%    {erl_syntax:get_pos(Data), ?MODULE, {Code, Data}}
%%    Error#error{errors = [{Code, Data} | Errors]}.
  Reason = {erl_syntax:get_pos(Node), Mod, {Code, Node}},
  Error#error{errors = [Reason | Reasons]}.

-spec push_warning(module(), detail(), error()) -> error().
push_warning(Mod, {Code, Node}, Error = #error{warnings = Reasons}) ->
  Reason = {erl_syntax:get_pos(Node), Mod, {Code, Node}},
  Error#error{warnings = [Reason | Reasons]}.


%%% ----------------------------------------------------------------------------
%%% Helper functions.
%%% ----------------------------------------------------------------------------

%% @private Prints the specified list of issues to standard_error.
-spec print_reasons(severity(), {file:filename(), reasons()}) -> any().
print_reasons(Severity, {Filename, Reasons}) ->
  Base = lists:last(filename:split(Filename)),
  [print_reason(Severity, Base, Reason) || Reason <- Reasons].

% With filename.
%% @private Prints the specified issue to standard_error.
-spec print_reason(severity(), file:name_all(), reason()) -> ok.
print_reason(Severity, Base, {ANNO, Mod, Detail}) ->
  io:format(
    standard_error, "1. ~ts:~w: ~ts: ~ts~n",
    [Base, ANNO, Severity, Mod:format_error(Detail)] % Delegate to the module showing the error.
  ).

% No filename and used to print lone errors.
-spec print_reason(severity(), reason()) -> ok.
print_reason(Severity, {ANNO, Mod, Detail}) ->
  io:format(
    standard_error, "2. ~w: ~ts: ~ts~n",
    [ANNO, Severity, Mod:format_error(Detail)] % Delegate to the module showing the error.
  );
print_reason(Severity, {Mod, Detail}) ->
  io:format(standard_error, "3. ~ts: ~ts~n",
    [Severity, Mod:format_error(Detail)] % Delegate to the module showing the error.
  ).