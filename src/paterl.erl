%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. May 2023 14:45
%%%-------------------------------------------------------------------
-module(paterl).
-author("duncan").

-compile(export_all).

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
%%-export([file/1, format_error/1]).


-spec file(file:name_all()) -> any().
file(File) when is_list(File) ->

  % Parse and preprocess file, applying macro expansions to the AST.
  case epp:open([{name, File}]) of
    {ok, Epp} ->
      case parse(Epp) of
        {ok, Forms} ->
%%          ?DEBUG("Got forms ~p", [Forms]);

        % 1. Check file. Should have its own error handling with unsupported.
          Errors = erl_subsyntax:check(Forms),

          % Extract relative file name for error reporting.
          FName = lists:last(filename:split(File)),
          IoList = [[FName, erl_subsyntax:format_err(Error), $\n] || Error <- Errors],
          file:write(standard_error, IoList);

        % 2. Build type_tbl. Should have its own error reporting.

        % 3. Expand the tree for the mailboxes.

        % 4. Analyse the regexes to infer the free clauses in guards.

        {error, Errors} ->
          errors:show_errors(File, Errors)
      end;
    {error, Error} ->
      errors:show_error({epp, Error})
  end.

parse(Epp) when is_pid(Epp) ->
  case parse_forms(Epp, [], []) of
    {Forms, []} ->
      {ok, lists:reverse(Forms)};
    {_, Errors} ->
      {error, lists:reverse(Errors)}
  end.

parse_forms(Epp, Forms, Errors) ->
  case epp:parse_erl_form(Epp) of
    {ok, AbsForm} ->
      parse_forms(Epp, [AbsForm | Forms], Errors);
    {error, ErrorInfo} ->
      parse_forms(Epp, Forms, [ErrorInfo | Errors]);
    {warning, WarningInfo} ->
      parse_forms(Epp, Forms, [WarningInfo | Errors]);
    Eof = {eof, _} ->
      {[Eof | Forms], Errors}
  end.









