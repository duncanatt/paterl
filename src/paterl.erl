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
%%-define(EXEC, "/Users/duncan/Dropbox/Postdoc/Development/mbcheck/mbcheck").
-define(EXEC, "/Users/duncan/Downloads/mbcheck/mbcheck -qj").

%%% Error types.

%% Pat error with message.
-define(E_PAT_MSG, e_pat_msg).

%% Unknown Pat error.
-define(E_PAT_NONE, e_pat_none).

%%TODO: Ultimate plan: Desugar -> ANF -> CPS -> RTL -> SSA -> RTL -> ASM

compile(File, Opts) when is_list(File), is_list(Opts) ->
  % Preprocess file.
  io:fwrite(color:green("[EPP] Preprocessing file ~s.~n"), [File]),

  io:fwrite(color:green("[SYNTAX] Sub-syntax checking.~n")),

  case epp:parse_file(File, Opts) of
    {ok, Forms} ->
      % File preprocessed.
      io:fwrite(color:green("[LINT] Linting.~n")),

      case erl_lint:module(Forms) of
        {ok, Warnings0} ->
          % File valid but possible warnings.
          errors:show_warnings(Warnings0),

          io:fwrite(color:green("[IR] IRing.~n")),
          Desugared = paterl_ir_3:module(Forms),
          io:format("~n~n~n D E S U G A R E D~n~n~n~n", []),
          pp_forms(Desugared),
          io:format("~n~n~n~p~n", [Desugared]),
          io:format("~n~n~n E N D D E S U G A R E D~n~n~n~n", []),
%%          init:stop(-1),
%%

          % Get program types table.
          io:fwrite(color:green("[TYPE] Extracting typespecs.~n")),

%%          case paterl_types:table(Forms) of
          case paterl_types:table(Desugared) of
            {ok, TInfo = #t_info{types = Types, specs = Specs, mb_defs = MbDefs, mb_names = MbNames}, Warnings1} ->
              % Type table valid but possible warnings.
              errors:show_warnings({File, Warnings1}),

              io:format("~n~s TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),
              io:format("Types: ~p~n", [Types]),
              io:format("Specs: ~p~n", [Specs]),
              io:format("MbDefs: ~p~n", [MbDefs]),
              io:format("MbNames: ~p~n", [MbNames]),
              io:format("~s SIGS & TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),

              {Desugared0, TInfo0} = paterl_bootstrap:forms(Desugared, TInfo),

              io:format("~n~s TINFO BOOTSTRAPPED~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),
              io:format("Types: ~p~n", [TInfo0#t_info.types]),
              io:format("Specs: ~p~n", [TInfo0#t_info.specs]),
              io:format("MbDefs: ~p~n", [TInfo0#t_info.mb_defs]),
              io:format("MbNames: ~p~n", [TInfo0#t_info.mb_names]),
              io:format("~s SIGS & TINFO BOOTSTRAPPED ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),


              io:format("~n~n~n D E S U G A R E D 2~n~n~n~n", []),
              pp_forms(Desugared0),
              io:format("~n~n~n~p~n", [Desugared0]),
              io:format("~n~n~n E N D D E S U G A R E D 2~n~n~n~n", []),


              % Annotate forms using type table.
              io:fwrite(color:green("[ANNOTATE] Annotating Erlang forms.~n")),
%%              case paterl_anno:annotate(Forms, TInfo) of
%%              case paterl_anno:annotate(Desugared0, TInfo0) of
%%              paterl_anno:annotate(Desugared0, TInfo0) == paterl_anno_3:annotate(Desugared0, TInfo0),
              case paterl_anno_3:annotate(Desugared0, TInfo0) of

                {ok, Annotated} ->
                  % Forms annotated.

                  io:format("~n~n~nOriginal forms:~n~p~n", [Forms]),
                  io:format("~n~n~nAnnotated forms:~n~p~n", [Annotated]),

                  io:fwrite(color:green("[COMPILE] Compiler sanity checks.~n")),
                  compile:forms(Annotated),

                  io:fwrite(color:green("[TRANSLATE] Translating Erlang forms to Pat.~n")),
%%                  PatAstTmp = paterl_trans_4:module(Annotated),
                  erase(),
                  PatAst = paterl_trans_5:module(Annotated),

%%                  PatAst = PatAstTmp,
                  io:fwrite("Pat AST: ~p~n~n", [PatAst]),
                  PatString = pat_prettypr:module(PatAst),
                  io:fwrite("Pat String:~n~n~s~n~n", [PatString]),

%%                  init:stop(),

                  io:format("~n~n~nOutput Pat:~n~n~n~s~n", [number(PatString)]),

                  PatFile = filename:rootname(File),
                  io:fwrite(color:green("[WRITE] Writing temporary Pat file ~s.~n"), [PatFile]),
                  case file:write_file(PatFile, PatString) of
                    ok ->
                      io:fwrite(color:green("[PAT] Pat'ting ~s.~n"), [PatFile]),
                      case exec(?EXEC ++ " " ++ PatFile) of
                        {0, _} ->
                          % Generated Pat file type-checked successfully.
                          ok;
                        {_, Bytes} ->
                          % Generated Pat file contains errors.
                          Msg = parse_error(Bytes),
                          errors:show_error({?MODULE, {?E_PAT_MSG, Msg}})
                      end;
                    {error, Error} -> % Reason
                      errors:show_error({file, Error})
                  end;
                #error{errors = Errors2} ->
%%                  io:format("Errors found: ~p", [Errors2]),
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
%% Adapted from: https://stackoverflow.com/questions/27028486/how-to-execute-system-command-in-erlang-and-get-results-using-oscmd-1
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

parse_error(Msg) ->
  case re:run(Msg, "\\[.*\\]\s(?P<A>.+)", [{capture, ['A'], list}, dotall]) of
    {match, Msg0} ->
      translate(sanitize(Msg0));
    nomatch ->
      Msg
  end.

sanitize(Msg) ->
  re:replace(
    re:replace(Msg, "[\r\n]", "", [global, {return, list}]),
    "\s+",
    " ",
    [global, {return, list}]
  ).

translate(Msg) ->
  case re:run(
    Msg,
    "^(?P<A>.*) is not included in (?P<B>.*)$", [{capture, ['A', 'B'], list}]) of
    {match, ["1", B]} ->
      % This was already a decent error message.
%%      io_lib:format(
%%        "Inferred omitted message send but expected user-asserted message pattern '~s'",
%%        [B]
%%      );
      % This is a better error message.
      io_lib:format(
        "Inferred an empty mailbox but expecting a missing message send that should produce the message pattern '~s'",
        [B]
      );
    {match, [A, "1"]} ->
      % This was already a decent error message.
%%      io_lib:format(
%%        "Inferred omitted message receive but expected user-asserted message pattern '~s'",
%%        [A]
%%      );
      % This is a better error message.
      io_lib:format(
        "Expecting an empty mailbox but inferred a missing message receive that should consume the message pattern '~s'",
        [A]
      );
    {match, [A, B]} ->
      io_lib:format(
%%        "Inferred message pattern '~s' is not included in user-asserted message pattern '~s'",
        "Inferred message pattern '~s' but expected user-asserted message pattern '~s'",
        [A, B]
      );
    nomatch ->
      Msg
  end.

number(Data) ->
  Lines = re:split(Data, "^", [{return, list}, multiline]),
%%  lists:zipwith(fun(I, Line) -> [integer_to_list(I), ": " | Line] end, lists:seq(1, length(Lines)), Lines).
  lists:zipwith(fun(I, Line) -> [io_lib:format("~3b: ", [I]) | Line] end, lists:seq(1, length(Lines)), Lines).


pp_forms(Forms) ->
  [io:fwrite(erl_pp:form(Form, [{indent, 2}])) || Form <- Forms].

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

