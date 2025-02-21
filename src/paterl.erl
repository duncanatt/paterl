-module(paterl).
-author("duncan").

%%-include("errors.hrl").
-include("paterl_lib.hrl").

%% API
-export([check/2]).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
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
-define(E_BAD__PAT, e_bad__pat).

%% Unknown Pat error.
-define(E_UNK__PAT, e_unk__pat).

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

-spec check(file:name(), list()) -> any().
check(File, Opts) when is_list(File), is_list(Opts) ->
  maybe
    {ok, Forms} ?= load_forms(File, Opts),
    {ok, AnfForms} ?= prep_forms(Forms),
    {ok, BootstrappedForms, TInfo} ?= type_forms(AnfForms),
    {ok, PatForms} ?= anno_forms(BootstrappedForms, TInfo),
    {ok, PatFile} ?= write_forms(File, Opts, PatForms),
    ok ?= check_pat(PatFile)
  end.


load_forms(File, Opts) ->
  maybe
  % Preprocess file.
    io:fwrite("[EPP] Preprocess file ~s.~n", [File]),
    {ok, Forms} ?= epp:parse_file(File, Opts),

    % Lint file, checking for valid syntax and that the syntax is in the Erlang
    % syntactic subset in the paper. Valid forms can still return possible
    % warnings.
    io:fwrite("[LINT] Module validity and syntactic subset check.~n"),
    {ok, Warnings0} ?= paterl_lint:module(Forms),
    paterl_errors:show_warnings(Warnings0),

    % Check well-formedness of mailbox interface definition attributes. Valid
    % forms can still return possible warnings.
    % TODO: To merge mailbox interface definition attribute check in the
    % TODO: paterl_lint module. Leave rest of module as is.
    {ok, Forms, Warnings1} ?= paterl_syntax:module(Forms),
    paterl_errors:show_warnings(Warnings1),

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
  % Generate ANF. To be done later.
  io:fwrite("[IR] ANF (eventually).~n"),
  Desugared = paterl_ir:module(Forms),

  io:format("~n~n~n D E S U G A R E D~n~n~n~n", []),
  pp_forms(Desugared),
  io:format("~n~n~n~p~n", [Desugared]),
  io:format("~n~n~n E N D D E S U G A R E D~n~n~n~n", []),

  {ok, Desugared}.

type_forms(Forms) ->
  % Extract type annotations. Valid forms can still return possible warnings.
  io:fwrite("[TYPE] Extract type annotations.~n"),
  case paterl_types:module(Forms) of
    {ok, TypeInfo, Warnings0} ->
      paterl_errors:show_warnings(Warnings0),

      % Create the main bootstrapping function in preparation for the Pat pass.
      {Forms0, TypeInfo0} = paterl_bootstrap:forms(Forms, TypeInfo),

      table_fmt:print_table("TypeDefs", TypeInfo0#type_info.type_defs),
      table_fmt:print_table("SpecDefs", TypeInfo0#type_info.spec_defs),
      table_fmt:print_table("MbFuns", TypeInfo0#type_info.mb_funs),
      table_fmt:print_table("MbDefs", TypeInfo0#type_info.mb_defs),

      {ok, Forms0, TypeInfo0};
    {error, Errors, Warnings} ->
      % Type annotation extraction errors.
      paterl_errors:show_warnings(Warnings),
      paterl_errors:show_errors(Errors),
      error
  end.



anno_forms(Forms, TypeInfo) ->
  maybe
  % Compute call graph.
    io:fwrite(color:green("[CALL GRAPH] Compute Erlang form call graph.~n")),
    {ok, CallGraph, []} ?= paterl_call_graph:module(Forms),
    table_fmt:print_table("CallGraph", CallGraph),

    % Compute the list of direct and mutual recursive fun references.
    RecFunInfo = paterl_call_graph:rec_funs(CallGraph),
    table_fmt:print_table("RecFunInfo", RecFunInfo),

    % Annotate Erlang forms.
    io:fwrite(color:green("[ANNOTATE] Annotate Erlang forms.~n")),
    {ok, AnnoForms, _} ?= paterl_anno:module(Forms, RecFunInfo, TypeInfo),

    % Compile sanity check.
    ?assertMatch({ok, _, _}, compile:forms(AnnoForms)),

    io:fwrite(color:green("[TRANSLATE] Translating Erlang forms to Pat.~n")),
    PatForms = paterl_trans:module(AnnoForms),

    io:fwrite("Pat AST: ~p~n~n", [PatForms]),
    {ok, PatForms}
  else
    {error, Errors, Warnings} ->
      % Type annotation errors.
      paterl_errors:show_warnings(Warnings),
      paterl_errors:show_errors(Errors),
      error
  end.



write_forms(File, Opts, PatForms) ->
  PatString = pat_prettypr:module(PatForms),
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
      paterl_errors:show_error({file, Error}),
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
      paterl_errors:show_error({?MODULE, {?E_BAD__PAT, Msg}}),
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
  lists:zipwith(fun(I, Line) -> [io_lib:format("~3b: ", [I]) | Line] end, lists:seq(1, length(Lines)), Lines).


pp_forms(Forms) ->
  [io:fwrite(erl_pp:form(Form, [{indent, 2}])) || Form <- Forms].


%%% ----------------------------------------------------------------------------
%%% Error handling and reporting.
%%% ----------------------------------------------------------------------------

%% @doc Formats the specified error to human-readable form.
format_error({?E_BAD__PAT, Reason}) ->
  io_lib:format(
    "~s",
    [Reason]
  );
format_error(?E_UNK__PAT) ->
  "Unknown Pat error; see generated output file".

