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
-include("paterl.hrl").

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
            {ok, TInfo = #t_info{types = Types, specs = Specs, mb_defs = MbDefs, mb_names = MbNames}, Warnings1} ->
              % Type table is valid.


              % Table valid but possible warnings.
              errors:show_warnings({File, Warnings1}),

              io:format("~n~s TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),
              io:format("Types: ~p~n", [Types]),
              io:format("Specs: ~p~n", [Specs]),
              io:format("MbDefs: ~p~n", [MbDefs]),
              io:format("MbNames: ~p~n", [MbNames]),
              io:format("~s SIGS & TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),


              case paterl_anno:annotate(Forms, TInfo) of

%%                {Annotated, Error2} = paterl_anno:annotate(Forms, TInfo),

                {ok, Annotated} ->
                  io:format("~n~n~nOriginal forms:~n~p~n", [Forms]),
                  io:format("~n~n~nAnnotated forms:~n~p~n", [Annotated]),

                  io:format("~n~n~nCompiler output:~n", []),
                  compile:forms(Annotated),

                  Pat = paterl_trans:translate(Annotated),
                  io:format("~n~n~nOutput Pat:~n~n~n~s~n", [Pat]);
                #error{errors = Errors2} ->
                  io:format("Errors found: ~p", [Errors2]),
                  % File contains mailbox annotation errors.
                  errors:show_errors({File, Errors2})
              end;


%%              ok;
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