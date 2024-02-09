%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% The Pat Erlang source-to-source translator.
%%% @end
%%% Created : 29. Jan 2024 15:19
%%%-------------------------------------------------------------------
-module(paterl).
-author("duncan").

-include("errors.hrl").

%% API
-export([compile/2]).

compile(File, Opts) when is_list(File), is_list(Opts) ->
  case epp:parse_file(File, Opts) of
    {ok, Forms} ->

      % File preprocessed.
      case erl_lint:module(Forms) of
        {ok, Warnings0} ->
          % File valid but possible warnings.
          errors:show_warnings(Warnings0),

          % Get program types table.
          case paterl_types:table(Forms) of
            {ok, _, Warnings1} ->
              % Type table is valid.


              % Table valid but possible warnings.
              errors:show_warnings({File, Warnings1}),
              ok;
            #error{errors = Errors1, warnings = Warnings1} ->
              % File contains errors.
%%              errors:show_warnings([{File, Warnings1}]),
%%              errors:show_errors([{File, Errors1}]),
              errors:show_warnings({File, Warnings1}),
              errors:show_errors({File, Errors1}),
              errors
          end;



        {error, Errors, Warnings} ->

          % File contains errors.
          errors:show_warnings(Warnings),
          errors:show_errors(Errors),
          error
      end,

      ok;
    {error, Error} ->

      % Preprocessor errors.
      errors:show_error({epp, Error}),
      error
  end.