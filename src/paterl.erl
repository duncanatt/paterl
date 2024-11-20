-module(paterl).
-author("duncan").

-include("errors.hrl").
-include("paterl.hrl").

%% API
-export([compile/2]).
-compile(export_all).

-include("log.hrl").

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%%% Generic constants.


%% Mailbox annotation primitive type.
-define(EXEC, "/Users/duncan/Dropbox/Postdoc/Development/mbcheck/mbcheck").
%%-define(EXEC, "/Users/duncan/Downloads/mbcheck/mbcheck -qj").

%%% Error types.

%% Pat error with message.
-define(E_PAT_MSG, e_pat_msg).

%% Unknown Pat error.
-define(E_PAT_NONE, e_pat_none).

%%TODO: Ultimate plan: Desugar -> ANF -> CPS -> RTL -> SSA -> RTL -> ASM

%% Current flow:
%% 1. epp:parse_file
%% 2. erl_lint:module
%% 3. paterl_ir:module/anf
%% 4. paterl_types:table
%% 5. paterl_bootstrap:forms
%% 6. paterl_anno:annotate
%% 7. paterl_trans:module
%% 8. pat_prettypr:module

%%-spec compile(file:name(), list()) -> any().
%%compile(File, Opts) when is_list(File), is_list(Opts) ->
%%  maybe
%%  % File preprocess.
%%    io:fwrite("[EPP] Preprocessing file ~s.~n", [File]),
%%    {ok, Forms} ?= epp:parse_file(File, Opts),
%%
%%    % Erlang syntactic subset check.
%%    io:fwrite("[SYNTAX] Erlang syntactic subset check.~n"),
%%    ok ?= paterl_syntax:module(Forms),
%%
%%    % File lint. File may be valid but can contain possible warnings.
%%    io:fwrite("[LINT] Lint.~n"),
%%    {ok, Warnings0} ?= erl_lint:module(Forms),
%%    errors:show_warnings(Warnings0),
%%
%%    % ANF generation.
%%    io:fwrite("[IR] ANF (eventually).~n"),
%%    Desugared = paterl_ir:module(Forms),
%%    io:format("~n~n~n D E S U G A R E D~n~n~n~n", []),
%%    pp_forms(Desugared),
%%    io:format("~n~n~n~p~n", [Desugared]),
%%    io:format("~n~n~n E N D D E S U G A R E D~n~n~n~n", []),
%%
%%    % Type annotation extraction. File may be valid but can contain possible warnings.
%%    io:fwrite("[TYPE] Type annotation extraction.~n"),
%%    {ok, TInfo = #t_info{types = Types, specs = Specs, mb_defs = MbDefs, mb_names = MbNames}, Warnings1} ?= paterl_types:table(Desugared),
%%    errors:show_warnings({File, Warnings1}),
%%
%%    io:format("~n~s TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),
%%    io:format("Types: ~p~n", [Types]),
%%    io:format("Specs: ~p~n", [Specs]),
%%    io:format("MbDefs: ~p~n", [MbDefs]),
%%    io:format("MbNames: ~p~n", [MbNames]),
%%    io:format("~s SIGS & TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),
%%
%%    {Desugared0, TInfo0} = paterl_bootstrap:forms(Desugared, TInfo),
%%
%%    io:format("~n~s TINFO BOOTSTRAPPED~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),
%%    io:format("Types: ~p~n", [TInfo0#t_info.types]),
%%    io:format("Specs: ~p~n", [TInfo0#t_info.specs]),
%%    io:format("MbDefs: ~p~n", [TInfo0#t_info.mb_defs]),
%%    io:format("MbNames: ~p~n", [TInfo0#t_info.mb_names]),
%%    io:format("~s SIGS & TINFO BOOTSTRAPPED ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),
%%
%%    io:format("~n~n~n D E S U G A R E D 2~n~n~n~n", []),
%%    pp_forms(Desugared0),
%%    io:format("~n~n~n~p~n", [Desugared0]),
%%    io:format("~n~n~n E N D D E S U G A R E D 2~n~n~n~n", []),
%%
%%
%%  else
%%    {error, Error} ->
%%      % Preprocessor error.
%%      errors:show_error({epp, Error}),
%%      error;
%%%%    {error, Errors} when is_list(Errors) ->
%%%%      error_in_syntax_checking; %TODO: See what to do..can case slplit?
%%    {error, Errors, Warnings} ->
%%
%%      % File contains linting errors.
%%      errors:show_warnings(Warnings),
%%      errors:show_errors(Errors),
%%      error
%%  end.


-spec compile(file:name(), list()) -> any().
compile(File, Opts) when is_list(File), is_list(Opts) ->
  maybe
    {ok, Forms} ?= load_forms(File, Opts),
    {ok, AnfForms} ?= prep_forms(Forms),
    {ok, BootstrappedForms, TInfo} ?= type_forms(File, AnfForms),
    {ok, PatForms} ?= anno_forms(File, BootstrappedForms, TInfo),
    {ok, PatFile} ?= write_forms(File, Opts, PatForms),
    ok ?= check_pat(PatFile)
  end.


load_forms(File, Opts) ->
  maybe
    % Preprocess file.
    io:fwrite("[EPP] Preprocess file ~s.~n", [File]),
    {ok, Forms} ?= epp:parse_file(File, Opts),

    % Lint file. File may be valid but can contain possible warnings.
    io:fwrite("[LINT] Lint.~n"),
    {ok, Warnings0} ?= erl_lint:module(Forms),
    paterl_errors:show_warnings(Warnings0),
    {ok, Forms}
  else
    {error, Error} ->
      % Preprocessor error.
      Reason = {epp, Error},
      paterl_errors:show_error(Reason),
      error;
    {error, Errors, Warnings} ->
      % Lint errors.
      paterl_errors:show_warnings(Warnings),
      paterl_errors:show_errors(Errors),
      error
  end.

prep_forms(Forms) ->
  % Syntax check file. Syntax must be in the syntactic subset supported by Pat
  % and mailbox interface definition attributes must be well-formed.
  io:fwrite("[SYNTAX] Check Erlang syntactic subset.~n"),
  case paterl_syntax:module(Forms) of
    {ok, Forms, Warnings} ->
      paterl_errors:show_warnings(Warnings),
      % Generate ANF. To be done later.
      io:fwrite("[IR] ANF (eventually).~n"),
      Desugared = paterl_ir:module(Forms),

      io:format("~n~n~n D E S U G A R E D~n~n~n~n", []),
      pp_forms(Desugared),
      io:format("~n~n~n~p~n", [Desugared]),
      io:format("~n~n~n E N D D E S U G A R E D~n~n~n~n", []),

      {ok, Desugared};
    {error, Errors, Warnings} ->
      % Syntax check errors.
      paterl_errors:show_warnings(Warnings),
      paterl_errors:show_errors(Errors),
      error
  end.

type_forms(File, Forms) ->
  % Extract type annotations. File may be valid but can contain possible
  % warnings.
  io:fwrite("[TYPE] Extract type annotations.~n"),
  case paterl_types:module(Forms) of
    {ok, TInfo = #type_info{types = Types, specs = Specs, mb_funs = MbDefs, mb_defs = MbNames}, Warnings0} ->
      paterl_errors:show_warnings(Warnings0), %TODO: Encapsulate this in the paterl_types module and use the file attribute.

      io:format("~n~s TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),
      io:format("Types: ~p~n", [Types]),
      io:format("Specs: ~p~n", [Specs]),
      io:format("MbDefs: ~p~n", [MbDefs]),
      io:format("MbNames: ~p~n", [MbNames]),
      io:format("~s SIGS & TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),

      {Desugared0, TInfo0} = paterl_bootstrap:forms(Forms, TInfo),

      io:format("~n~s TINFO BOOTSTRAPPED~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),
      io:format("Types: ~p~n", [TInfo0#type_info.types]),
      io:format("Specs: ~p~n", [TInfo0#type_info.specs]),
      io:format("MbDefs: ~p~n", [TInfo0#type_info.mb_funs]),
      io:format("MbNames: ~p~n", [TInfo0#type_info.mb_defs]),
      io:format("~s SIGS & TINFO BOOTSTRAPPED ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),

      io:format("~n~n~n D E S U G A R E D 2~n~n~n~n", []),
      pp_forms(Desugared0),
      io:format("~n~n~n~p~n", [Desugared0]),
      io:format("~n~n~n E N D D E S U G A R E D 2~n~n~n~n", []),
      {ok, Desugared0, TInfo0};
    {error, Errors, Warnings} ->
      % Type annotation extraction errors.
      %TODO: Encapsulate this in the paterl_types module and use the file attribute.
      paterl_errors:show_warnings(Warnings),
      paterl_errors:show_errors(Errors),
      error
  end.



anno_forms(File, Forms, TInfo) ->
  % Annotate Erlang forms.
  io:fwrite(color:green("[ANNOTATE] Annotate Erlang forms.~n")),
  case paterl_anno:module(Forms, TInfo) of
    {ok, Annotated} ->
      io:format("~n~n~nOriginal forms:~n~p~n", [Forms]),
      io:format("~n~n~nAnnotated forms:~n~p~n", [Annotated]),

      io:fwrite(color:green("[COMPILE] Compile sanity check.~n")),
      {ok, _, _} = compile:forms(Annotated),

      io:fwrite(color:green("[TRANSLATE] Translating Erlang forms to Pat.~n")),
      PatForms = paterl_trans:module(Annotated),

      io:fwrite("Pat AST: ~p~n~n", [PatForms]),
      {ok, PatForms};
    {error, Errors, Warnings} ->
      % Type annotation errors.
      %TODO: Encapsulate this in the paterl_types module and use the file attribute.
      paterl_errors:show_warnings([{File, Warnings}]),
      paterl_errors:show_errors([{File, Errors}]),
      error
  end.


write_forms(File, Opts, PatForms) ->
  PatString = pat_prettypr:module(PatForms),
  io:fwrite("Pat String:~n~n~s~n~n", [PatString]),

  io:format("~n~n~nOutput Pat:~n~n~n~s~n", [number(PatString)]),

  OutDir = proplists:get_value(out, Opts),
  io:format("Ensuring that the directory exists: ~p.~n", [OutDir]),
  filelib:ensure_path(OutDir),

  PatFile = filename:join(OutDir, filename:basename(File, ".erl")),
  io:fwrite(color:green("[WRITE] Writing temporary Pat file ~s.~n"), [PatFile]),

  case file:write_file(PatFile, PatString) of
    ok ->
      {ok, PatFile};
    {error, Error} ->
      errors:show_error({file, Error}),
      error
  end.


check_pat(PatFile) ->
  io:fwrite(color:green("[PAT] Patt'ing ~s.~n"), [PatFile]),
  case exec(?EXEC ++ " " ++ PatFile) of
    {0, _} ->
      % Generated Pat file type-checked successfully.
      io:fwrite(color:green("[PAT] Successfully type-checked ~s.erl.~n~n~n"), [PatFile]);
    {_, Bytes} ->
      % Generated Pat file contains errors.
      Msg = parse_error(Bytes),
      errors:show_error({?MODULE, {?E_PAT_MSG, Msg}}),
      error
  end.



compile2(File, Opts) when is_list(File), is_list(Opts) ->
  % Preprocess file.
  io:fwrite(color:green("[EPP] Preprocessing file ~s.~n"), [File]),

  case epp:parse_file(File, Opts) of
    {ok, Forms} ->

      % File preprocessed. Check that Erlang syntax is supported by paterl.
      io:fwrite(color:green("[SYNTAX] Erlang Sub-syntax checking.~n")),
      case paterl_syntax:module(Forms) of
        {ok, _} ->

          % Erlang syntax in the supported subset.
          io:fwrite(color:green("[LINT] Linting.~n")),

          case erl_lint:module(Forms) of
            {ok, Warnings0} ->
              % File valid but possible warnings.
              errors:show_warnings(Warnings0),

              io:fwrite(color:green("[IR] IRing.~n")),
              Desugared = paterl_ir:module(Forms),
              io:format("~n~n~n D E S U G A R E D~n~n~n~n", []),
              pp_forms(Desugared),
              io:format("~n~n~n~p~n", [Desugared]),
              io:format("~n~n~n E N D D E S U G A R E D~n~n~n~n", []),
%%          init:stop(-1),
%%

              % Get program types table.
              io:fwrite(color:green("[TYPE] Extracting typespecs.~n")),

%%          case paterl_types:table(Forms) of
              case paterl_types:module(Desugared) of
                {ok, TInfo = #type_info{types = Types, specs = Specs, mb_funs = MbDefs, mb_defs = MbNames}, Warnings1} ->
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
                  io:format("Types: ~p~n", [TInfo0#type_info.types]),
                  io:format("Specs: ~p~n", [TInfo0#type_info.specs]),
                  io:format("MbDefs: ~p~n", [TInfo0#type_info.mb_funs]),
                  io:format("MbNames: ~p~n", [TInfo0#type_info.mb_defs]),
                  io:format("~s SIGS & TINFO BOOTSTRAPPED ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),


                  io:format("~n~n~n D E S U G A R E D 2~n~n~n~n", []),
                  pp_forms(Desugared0),
                  io:format("~n~n~n~p~n", [Desugared0]),
                  io:format("~n~n~n E N D D E S U G A R E D 2~n~n~n~n", []),


                  % Annotate forms using type table.
                  io:fwrite(color:green("[ANNOTATE] Annotating Erlang forms.~n")),
                  case paterl_anno:module(Desugared0, TInfo0) of
                    {ok, Annotated} ->
                      % Forms annotated.

                      io:format("~n~n~nOriginal forms:~n~p~n", [Forms]),
                      io:format("~n~n~nAnnotated forms:~n~p~n", [Annotated]),

                      io:fwrite(color:green("[COMPILE] Compiler sanity checks.~n")),
                      compile:forms(Annotated),

                      io:fwrite(color:green("[TRANSLATE] Translating Erlang forms to Pat.~n")),
%%                  PatAstTmp = paterl_trans_4:module(Annotated),
                      erase(),
                      PatAst = paterl_trans:module(Annotated),

%%                  PatAst = PatAstTmp,
                      io:fwrite("Pat AST: ~p~n~n", [PatAst]),
                      PatString = pat_prettypr:module(PatAst),
                      io:fwrite("Pat String:~n~n~s~n~n", [PatString]),

%%                  init:stop(),

                      io:format("~n~n~nOutput Pat:~n~n~n~s~n", [number(PatString)]),

%%                  PatFile = filename:rootname(File),
                      OutDir = proplists:get_value(out, Opts),
                      io:format("Ensuring that the directory exists: ~p.~n", [OutDir]),
                      filelib:ensure_path(OutDir),

%%                  PatFile = filename:join(OutDir, filename:rootname(File)),
                      PatFile = filename:join(OutDir, filename:basename(File, ".erl")),
                      io:fwrite(color:green("[WRITE] Writing temporary Pat file ~s.~n"), [PatFile]),


                      case file:write_file(PatFile, PatString) of
                        ok ->
                          io:fwrite(color:green("[PAT] Patt'ing ~s.~n"), [PatFile]),
                          case exec(?EXEC ++ " " ++ PatFile) of
                            {0, _} ->
                              % Generated Pat file type-checked successfully.
                              io:fwrite(color:green("[PAT] Successfully type-checked ~s.erl.~n~n~n"), [PatFile]);
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
          end;

        #error{errors = _} ->
          error_in_syntax_checking
      end;
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
        "Inferred from code an empty mailbox but expecting a missing message send that should produce the message pattern '~s'",
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
        "Expecting an empty mailbox but inferred from code a missing message receive that should consume the message pattern '~s'",
        [A]
      );
    {match, [A, B]} ->
      io_lib:format(
%%        "Inferred message pattern '~s' is not included in user-asserted message pattern '~s'",
        "Inferred from code message pattern '~s' but expected user-asserted message pattern '~s'",
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

