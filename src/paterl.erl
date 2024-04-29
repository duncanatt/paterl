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
-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%%% Generic constants.

%% Mailbox annotation primitive type.
-define(EXEC, "/Users/duncan/Dropbox/Postdoc/Development/mbcheck/mbcheck").

%%% Error types.

%% Pat error with message.
-define(E_PAT_MSG, e_pat_msg).

%% Unknown Pat error.
-define(E_PAT_NONE, e_pat_none).

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
              % Type table valid but possible warnings.
              errors:show_warnings({File, Warnings1}),

              io:format("~n~s TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),
              io:format("Types: ~p~n", [Types]),
              io:format("Specs: ~p~n", [Specs]),
              io:format("MbDefs: ~p~n", [MbDefs]),
              io:format("MbNames: ~p~n", [MbNames]),
              io:format("~s SIGS & TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),

              % Annotate forms TODO: Update comment.
              case paterl_anno:annotate(Forms, TInfo) of

                {ok, Annotated} ->
                  io:format("~n~n~nOriginal forms:~n~p~n", [Forms]),
                  io:format("~n~n~nAnnotated forms:~n~p~n", [Annotated]),

                  io:format("~n~n~nCompiler output:~n", []),
                  compile:forms(Annotated),

                  Pat = paterl_trans:translate(Annotated),
                  io:format("~n~n~nOutput Pat:~n~n~n~s~n", [Pat]),


                  PatFile = filename:rootname(File),
                  case file:write_file(PatFile, Pat) of
                    ok ->
                      case exec(?EXEC ++ " " ++ PatFile) of
                        {0, _} ->
                          % Generated Pat file type-checked successfully.
                          ok;
                        {_, Bytes} ->
                          % Generated Pat file contains type errors.
                          case re:split(lists:flatten(Bytes), "^\\[.*\\] (.*)$", [trim, {return, list}]) of
                            [[]] ->
                              % Pat error output empty.
                              errors:show_error({?MODULE, ?E_PAT_NONE});
                            [[] | Msg] ->
                              % Defined Pat error.
                              errors:show_error({?MODULE, {?E_PAT_MSG, Msg}})
                          end
                      end;
                    {error, Error} -> % Reason
                      errors:show_error({file, Error})
                  end;
                #error{errors = Errors2} ->
                  io:format("Errors found: ~p", [Errors2]),
                  % File contains mailbox annotation errors.
                  errors:show_errors({File, Errors2})
              end;
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

%% @private Executes specified command on shell and returns the status and
%% command output.
%%
%% Call is synchronous.
-spec exec(string()) -> iolist().
exec(Cmd) ->
  Port = open_port(
    {spawn, Cmd},
    [stream, in, eof, hide, exit_status, stderr_to_stdout]
  ),
  get_output(Port, []).

%% @private Retrieves the command output and closes the port.
-spec get_output(port(), iolist()) -> iolist().
get_output(Port, Read) ->
  receive
    {Port, {data, Bytes}} ->
      get_output(Port, [Read | Bytes]);
    {Port, eof} ->
      port_close(Port),
      Status =
        receive
          {Port, {exit_status, Code}} ->
            Code
        end,
      {Status, Read}
  end.

%%% ----------------------------------------------------------------------------
%%% Error handling and reporting.
%%% ----------------------------------------------------------------------------

%% @doc Formats the specified error to human-readable form.
format_error({?E_PAT_MSG, Reason}) ->
  io_lib:format(
    "~s",
    [Reason]
  );
format_error(?E_PAT_NONE) ->
  "Unknown Pat error; see generated output file".
