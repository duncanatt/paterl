%%
%% %CopyrightBegin%
%%
%% Copyright the University of Glasgow 2022-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(paterl).
-moduledoc "Erlang mailbox type checker.".
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").
-include("paterl_lib.hrl").

%%% Public API.
-export([check/2, format_error/1]).

%%% Public types.
-export_type([opt_includes/0, opt_out/0, opt_verbose/0, opt_skip/0, options/0]).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%%% Generic constants.

%% Mailbox annotation primitive type.
-define(EXEC, "/Users/duncan/Dropbox/Postdoc/Development/mbcheck/mbcheck -qj").
%%-define(EXEC, "/Users/duncan/Downloads/mbcheck/mbcheck -qj").

%% Pat file extension.
-define(PAT_EXT, ".pat").

%%% Command line arguments.

% Erlang file name.
-define(OPT_FILE, file).

% Erlang include directories.
-define(OPT_INCLUDES, includes).

% Pat generated output directory.
-define(OPT_OUT, out).

% Verbosity level.
-define(OPT_VERBOSE, verbose).

% Skip Pat typechecking flag.
-define(OPT_SKIP, skip).

%%% Error types.

%% Pat error with message.
-define(E_BAD__PAT, e_bad__pat).

%% Unknown Pat error.
-define(E_UNK__PAT, e_unk__pat).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-doc "Erlang include directories.".
-type opt_includes() :: {?OPT_INCLUDES, [file:name_all()]}.

-doc "Pat generated output directory".
-type opt_out() :: {?OPT_OUT, file:name_all()}.

-doc "Verbosity level.".
-type opt_verbose() :: {?OPT_VERBOSE, all | none | ssa | types | anno | pat}.

-doc "Skip Pat typechecking flag.".
-type opt_skip() :: {?OPT_SKIP, boolean()}.

-doc "Module options.".
-type options() :: opt_includes() | opt_out() | opt_verbose() | opt_skip().


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

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

-doc """
Type checks an Erlang module for mailbox type errors.

Type checking requires the Pat toolchain to be set up correctly.

Errors and warnings are printed on the shell.

### Returns
- `ok` if type checking succeeds
- `error` otherwise
""".
-spec check(File, Opts) -> ok | error
  when
  File :: file:name(),
  Opts :: options().
check(File, Opts) when is_list(File), is_list(Opts) ->
  maybe
  % Type checking pipeline.
    {ok, Forms} ?= load_forms(File, Opts),
    {ok, SsaForms} ?= prep_forms(Forms, Opts),
    {ok, BootstrappedForms, TInfo} ?= type_forms(SsaForms, Opts),
    {ok, PatForms} ?= anno_forms(BootstrappedForms, TInfo, Opts),
    {ok, PatFile} ?= write_forms(File, PatForms, Opts),
    ok ?= check_pat(PatFile, Opts)
  end.


%%% ----------------------------------------------------------------------------
%%% Type checking pipeline.
%%% ----------------------------------------------------------------------------

-doc "Loads an Erlang module.".
-spec load_forms(File, Opts) -> {ok, Forms} | error
  when
  File :: file:name(),
  Opts :: options(),
  Forms :: paterl_syntax:forms().
load_forms(File, Opts) ->
  maybe
  % Preprocess file.
    io_util:info("[EPP] Preprocess file ~s.", [File]),
    {ok, Forms} ?= epp:parse_file(File, Opts),

    % Lint file, checking for valid syntax and that the syntax is in the Erlang
    % syntactic subset in the paper. Valid forms can still return possible
    % warnings.
    io_util:info("[LINT] Module validity and syntactic subset check."),
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

-doc "Prepares Erlang forms by transforming them to SSA.".
-spec prep_forms(Forms, Opts) -> {ok, SsaForms} | error
  when
  Forms :: paterl_syntax:forms(),
  Opts :: options(),
  SsaForms :: paterl_syntax:forms().
prep_forms(Forms, Opts) ->
  Verbose = proplists:get_value(?OPT_VERBOSE, Opts),

  % Generate SSA. ANF to be done later.
  io_util:info("[IR] SSA (ANF too, eventually)."),
  SsaForms = paterl_ir:module(Forms),

  if Verbose =:= all; Verbose =:= ssa ->
    io_util:banner("SSA forms"),
    io_util:log("~p~n", [SsaForms]),
    io_util:banner("SSA code"),
    pp_forms(SsaForms);
    true -> ok
  end,

  {ok, SsaForms}.

-doc """
Adds a bootstrapping main function to `Forms` and extracts the type information.

The type information consists of:
1. Type definitions
2. Type spec definitions
3. Functions with associated mailbox annotations
4. Mailbox definitions
""".
-spec type_forms(Forms, Opts) -> {ok, BsForms, TypeInfo} | error
  when
  Forms :: paterl_syntax:forms(),
  Opts :: options(),
  BsForms :: paterl_syntax:forms(),
  TypeInfo :: paterl_types:type_info().
type_forms(Forms, Opts) ->
  Verbose = proplists:get_value(?OPT_VERBOSE, Opts),

  % Extract type annotations. Valid forms can still return possible warnings.
  io_util:info("[TYPE] Extract type annotations."),
  case paterl_types:module(Forms) of
    {ok, TypeInfo, Warnings0} ->
      paterl_errors:show_warnings(Warnings0),

      % Create the main bootstrapping function in preparation for the Pat pass.
      {BsForms, TypeInfo0} = paterl_bootstrap:module(Forms, TypeInfo),

      if Verbose =:= all; Verbose =:= types ->
        io_util:table("TypeDefs", TypeInfo0#type_info.type_defs),
        io_util:table("SpecDefs", TypeInfo0#type_info.spec_defs),
        io_util:table("MbFuns", TypeInfo0#type_info.mb_funs),
        io_util:table("MbDefs", TypeInfo0#type_info.mb_defs);
        true -> ok
      end,

      {ok, BsForms, TypeInfo0};
    {error, Errors, Warnings} ->
      % Type annotation extraction errors.
      paterl_errors:show_warnings(Warnings),
      paterl_errors:show_errors(Errors),
      error
  end.

-doc """
Annotates `Forms` with the type information

Type annotations are placed inside the annotation [`anno()`](`t:erl_anno:anno/0`)
of Erlang syntax nodes. While this works, it technically violates the
[`anno()`](`t:erl_anno:anno/0`) type specification, which does not permit extra
annotations besides the ones defined in [`anno()`](`t:erl_anno:anno/0`).
""".
-spec anno_forms(Forms, TypeInfo, Opts) -> {ok, PatForms}
  when
  Forms :: paterl_syntax:forms(),
  TypeInfo :: paterl_types:type_info(),
  Opts :: options(),
  PatForms :: pat_syntax:forms().
anno_forms(Forms, TypeInfo, Opts) ->
  Verbose = proplists:get_value(?OPT_VERBOSE, Opts),

  maybe
  % Compute call graph.
    io_util:info("[CALL GRAPH] Compute Erlang form call graph."),
    {ok, CallGraph, []} ?= paterl_call_graph:module(Forms),
    if Verbose =:= all; Verbose =:= anno ->
      io_util:table("CallGraph", CallGraph);
      true -> ok
    end,

    % Compute the list of direct and mutual recursive fun references.
    RecFunInfo = paterl_call_graph:rec_funs(CallGraph),
    if Verbose =:= all; Verbose =:= anno ->
      io_util:table("RecFunInfo", RecFunInfo);
      true -> ok
    end,

    % Annotate Erlang forms.
    io_util:info("[ANNOTATE] Annotate Erlang forms."),
    {ok, AnnoForms, _} ?= paterl_anno:module(Forms, RecFunInfo, TypeInfo),

    % Compile sanity check.
    ?assertMatch({ok, _, _}, compile:forms(AnnoForms)),
    if Verbose =:= all; Verbose =:= anno ->
      io_util:banner("Annotated Erlang forms"),
      io_util:log("~p", [AnnoForms]);
      true -> ok
    end,

    io_util:info("[TRANSLATE] Translating Erlang forms to Pat."),
    PatForms = paterl_trans:module(AnnoForms),
    if Verbose =:= all; Verbose =:= anno ->
      io_util:log("Pat AST: ~p~n~n", [PatForms]);
      true -> ok
    end,

    {ok, PatForms}
  else
    {error, Errors, Warnings} ->
      % Type annotation errors.
      paterl_errors:show_warnings(Warnings),
      paterl_errors:show_errors(Errors),
      error
  end.

-doc "Writes `PatForms` to the specified file.".
-spec write_forms(File, PatForms, Opts) -> {ok, PatFile} | error
  when
  File :: file:name(),
  PatForms :: pat_syntax:forms(),
  Opts :: options(),
  PatFile :: file:name().
write_forms(File, PatForms, Opts) ->
  Verbose = proplists:get_value(?OPT_VERBOSE, Opts),

  % Pretty print Pat AST to generate Pat code.
  PatString = pat_prettypr:module(PatForms),
  if Verbose =:= all; Verbose =:= pat ->
    io_util:banner("Pat code"),
    io_util:log("~s", [number(PatString)]);
    true -> ok
  end,

  OutDir = proplists:get_value(out, Opts),
  io:format("Ensuring that the directory exists: ~p.~n", [OutDir]),
  filelib:ensure_path(OutDir),

  PatFile = filename:join(OutDir, filename:basename(File, ".erl")) ++ ?PAT_EXT,
  io_util:info("[WRITE] Writing temporary Pat file ~s.", [PatFile]),

  case file:write_file(PatFile, PatString) of
    ok ->
      {ok, PatFile};
    {error, Error} ->
      paterl_errors:show_error({file, Error}),
      error
  end.

-doc "Runs the Pat mailbox type checking tool on the specified `PatFile`.".
-spec check_pat(PatFile, Opts) -> ok | error
  when
  PatFile :: file:name(),
  Opts :: options().
check_pat(PatFile, Opts) ->
  Skip = proplists:get_bool(?OPT_SKIP, Opts),
  if not Skip ->
    io_util:info("[PAT] Patt'ing ~s.", [PatFile]),
    case exec(?EXEC ++ " " ++ PatFile) of
      {0, _} ->
        % Generated Pat file type-checked successfully.
        io_util:info("[PAT] Successfully type-checked ~s.erl.~n~n", [PatFile]);
      {_, Bytes} ->
        % Generated Pat file contains errors.
        Msg = parse_error(Bytes),
        paterl_errors:show_error({?MODULE, {?E_BAD__PAT, Msg}}),
        error
    end;
    true ->
      ok
  end.

-doc """
Executes the specified command on the shell and returns the status and returns
the command output.

The execution is synchronous and blocks the caller.
""".
-spec exec(Cmd) -> Bytes
  when
  Cmd :: string(),
  Bytes :: iolist().
exec(Cmd) ->
  %% Adapted from: https://stackoverflow.com/questions/27028486/how-to-execute-system-command-in-erlang-and-get-results-using-oscmd-1
  Port = open_port(
    {spawn, Cmd},
    [stream, in, eof, hide, exit_status, stderr_to_stdout]
  ),
  get_output(Port, []).

-doc "Retrieves the command output and closes the port.".
-spec get_output(Port, Read) -> Read0
  when
  Port :: port(),
  Read :: iolist(),
  Read0 :: iolist().
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

-doc "Parses the errors returned by the Pat mailbox type checking tool.".
-spec parse_error(Msg) -> Msg0
  when
  Msg :: iolist(),
  Msg0 :: iolist().
parse_error(Msg) ->
  case re:run(Msg, "\\[.*\\]\s(?P<A>.+)", [{capture, ['A'], list}, dotall]) of
    {match, Msg0} ->
      translate(sanitize(Msg0));
    nomatch ->
      Msg
  end.

-doc """
Removes all carriage return and new line characters, and squashes multiple
space characters to a single space. Result is returned as a possibly deep list
of characters.
""".
-spec sanitize(Msg) -> Msg0
  when
  Msg :: iolist(),
  Msg0 :: iolist().
sanitize(Msg) ->
  re:replace(
    re:replace(Msg, "[\r\n]", "", [global, {return, list}]),
    "\s+",
    " ",
    [global, {return, list}]
  ).

-doc """
Translates the somewhat cryptic error messages returned by the Pat constraint
solving phase to human readable messages.
""".
-spec translate(Msg) -> Msg0
  when
  Msg :: iolist(),
  Msg0 :: iolist().
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
        "Inferred message pattern from code '~s' but expected user-asserted message pattern '~s'",
        [A, B]
      );
    nomatch ->
      Msg
  end.

-doc "Prepends the specified data with line numbers.".
-spec number(Data) -> Data0
  when
  Data :: iolist(),
  Data0 :: iolist().
number(Data) ->
  Lines = re:split(Data, "^", [{return, list}, multiline]),
  lists:zipwith(fun(I, Line) -> [io_lib:format("~3b: ", [I]) | Line] end, lists:seq(1, length(Lines)), Lines).

-doc "Pretty prints Erlang forms.".
-spec pp_forms(Forms :: paterl_syntax:forms()) -> iolist().
pp_forms(Forms) ->
  [io:fwrite(erl_pp:form(Form, [{indent, 2}])) || Form <- Forms].


%%% ----------------------------------------------------------------------------
%%% Error handling and reporting.
%%% ----------------------------------------------------------------------------

-doc """
Formats `Detail` to human-readable form.

See `module/1` for error and warnings codes.

### Returns
- message as a possibly deep [`io_lib:chars()`](`t:io_lib:chars/0`) list
""".
-spec format_error(Detail :: paterl_lib:detail()) -> io_lib:chars().
format_error({?E_BAD__PAT, Reason}) ->
  io_lib:format(
    "~s",
    [Reason]
  );
format_error(?E_UNK__PAT) ->
  "Unknown Pat error; see generated output file".

