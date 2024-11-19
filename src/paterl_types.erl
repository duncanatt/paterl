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
-module(paterl_types).
-moduledoc "Erlang mailbox and type annotation extraction.".
-author("duncan").

%%% Includes.
-include("log.hrl").
-include("paterl_lib.hrl").
%%-include("errors.hrl").
-include("paterl.hrl").

%%% Imports.
-import(erl_syntax, [
type/1,
concrete/1,
get_pos/1,
copy_pos/2,
attribute_name/1,
function_name/1,
function_arity/1,
erl_parse/0,
atom/1,
integer/1,
implicit_fun/2,
revert/1,
set_pos/2
]).

%%% Public API.
-export([table/1]).

%%% Public types.
-export_type([types/0, specs/0, mb_defs/0, mailbox/0, mb_names/0, t_info/0, signature/0]).
-export_type([result/0]).

-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%%% Error types.

%% Mailbox specification syntactically incorrect.
%%-define(E__BAD_MB_DEF, e_mb_spec_bad).

%% Invalid function Fun/Arity signature.
%%-define(E_MB_SIG_BAD, e_mb_sig_bad).

%% Untyped function signature. e_mb_sig_type_undef
-define(E_UNDEF__FUN_SPEC, e_undef__fun_spec).

%% Function signature associated with more than one mailbox type.
-define(E_MB_SIG_NOT_UNIQUE, e_mb_sig_not_unique).

%% Undefined function reference.
-define(E_UNDEF__FUN_REF, e_undef__fun_ref).

%% Undefined mailbox type.
-define(E_UNDEF__MB_TYPE, e_undef__mb_type).

%% Invalid message tag.
-define(E_BAD__MSG_TAG, e_bad__msg_tag).

%% Invalid message element type. Supported types are integers and strings.
-define(E_BAD__MSG_ELEM_TYPE, e_bad__msg_elem_type).

%% No pid() associated with mailbox type.
-define(W_NO__MB_FUN_REF, w_no__mb_fun_ref).

%% No pid() associated with mailbox type.
-define(W_NO__PID, w_no__pid).

%% Mailbox not initialized with new modality.
-define(E_NO__MB_NEW, e_no__mb_new).

%% User-defined mailbox interface type contains types other than a message.
-define(E_BAD__MSG_TYPE, e_bad__msg_type).

%% User-defined message type contains types other than a built-in or a
%% user-defined mailbox interface type.
-define(E_BAD__MB_TYPE, e_bad__mb_type).

%% Undefined type.
-define(E__UNDEF_TYPE, e__undef_type).

%% Error macros.

%% Program type information.
%%-record(p_types, {
%%  t_specs = [] :: [t_spec()], % Global type def names to AST.
%%  f_specs = [] :: [f_spec()], % Function signatures to AST.
%%  mb_sigs = [] :: [mb_sig()]  % Mailbox names to mailbox modality and function signatures.
%%}).


%%-record(t_info, {
%%  types = #{} :: types(), % Global type def names to AST.
%%  specs = #{} :: specs(), % Function signatures to AST.
%%  mb_defs = #{} :: mb_defs(),  % Mailbox names to mailbox modality and function signatures.
%%  mb_names = [] :: [mailbox()]
%%}).

%%-record(p_data, {
%%  f_sigs = [] :: [signature()] %% List of function signatures.
%%}).

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type signature() :: {name(), arity()}.
%% Function signature Name/Arity type.

-type name() :: atom().
%% Function name type.

-type type() :: atom().
%% Type name type.

-type mailbox() :: atom().
%% Mailbox name type.

-type modality() :: ?M_NEW | ?M_USE.
%% Mailbox usage modality type.

-type anno() :: erl_anno:anno().

-doc """
Function result.
""".
%%-type result() :: {ok, term(), errors:warnings()} | {ok, errors:warnings()} | errors:error().

%%-type error() :: paterl_lib:error().

-record(form_info, {
  fun_refs = [],
  types = [],
  specs = [],
  mb_defs = []
}).


-type types() :: #{Name :: type() => {
  type | mbox,
  ANNO :: anno(),
  Type :: erl_parse:abstract_type(),
  TypeVars :: [erl_parse:abstract_expr()]}
}.

-type specs() :: #{signature() => {
  spec,
  ANNO :: anno(),
  FunTypes :: [erl_parse:abstract_type()]}
}.

-type mb_defs() :: #{signature() => {modality(), ANNO :: anno(), mailbox()}}.

-type mb_names() :: #{mailbox() => {ANNO :: anno(), modality()}}.


%% Mailbox interface names map that tracks the Erlang functions implementing the
%% mailbox type.

-type t_info() :: #t_info{}.
%% Program type information type.

-type result() :: {ok, t_info(), Warnings :: paterl_errors:warnings()} |
{errors, Warnings :: paterl_errors:warnings(), Errors :: paterl_errors:errors()}.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------


-spec table(erl_syntax:forms()) -> result().
table2(Forms) when is_list(Forms) ->
  _File = paterl_syntax:get_file(Forms),

  A =

    % Extract type information from AST to table.
  case get_t_info(Forms) of
    #analysis{status = ok, result = TInfo} ->
%%        {ok, TInfo = #t_info{}, []} ->

      #t_info{types = Types, specs = _Specs, mb_defs = _MbDefs, mb_names = MbNames} = TInfo,

%%          io:format("~n~s SIGS & TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),
%%          io:format("Types: ~p~n", [Types]),
%%          io:format("Specs: ~p~n", [Specs]),
%%          io:format("MbDefs: ~p~n", [MbDefs]),
%%          io:format("MbNames: ~p~n", [MbNames]),
%%          io:format("~s SIGS & TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),


      case check_mb_types_defined(MbNames, Types) of
%%            {ok, []} ->
        #analysis{status = ok} ->
          case check_mb_types_valid(Types) of
            #analysis{status = ok, warnings = Warnings} ->
%%                {ok, Warnings} ->

              %% TODO: Check mb is new (we have no use without new). Uses MbNames.
              case check_mb_new(MbNames) of
%%                    {ok, []} ->
                #analysis{status = ok} ->

                  #analysis{
                    file = paterl_syntax:get_file(Forms),
                    result = TInfo,
                    warnings = Warnings
                  };
%%                      {ok, TInfo, Warnings}; % TODO: Here we should create the analysis record and return_pack it.
                Analysis = #analysis{status = error} ->
                  % One or more mailbox definitions are used without new.
                  Analysis
              end;
            Analysis = #analysis{status = error} ->
              % One or more invalid messages types in mailbox definitions.
              Analysis
          end;
        Analysis = #analysis{status = error} ->
          % One or more undefined mailbox type names.
          Analysis
      end;
    Analysis = #analysis{status = error} ->
      % One or more function signatures associated with the same mailbox
      % definition or undeclared function signatures in mailbox definitions.
      Analysis
  end,

  paterl_lib:return(A).

table(Forms) when is_list(Forms) ->
  Anal =
    maybe
    % Extract type, spec, and mailbox definition information.
      #analysis{status = ok, result = TInfo} ?= get_t_info(Forms),
      #t_info{types = Types, mb_names = MbNames} = TInfo,

      % Check that mailbox interface definitions have corresponding types.
      #analysis{status = ok} ?= check_mb_types_defined(MbNames, Types),

      % Check validity of mailbox interface types.
      #analysis{status = ok, warnings = Warnings} ?= check_mb_types_valid(Types),

      % Check that mailbox interface uses are initialized with new.
      #analysis{status = ok} ?= check_mb_new(MbNames),

      #analysis{
        file = paterl_syntax:get_file(Forms),
        result = TInfo,
        warnings = Warnings
      }
    else
      Analysis = #analysis{status = error} ->
        Analysis
    end,
  paterl_lib:return(Anal).


% {Sigs, MbSigs, Types, Specs}

-spec read_attribs(erl_syntax:forms()) -> {FunSigs, MbDefs, Types, FunSpecs}
  when
  FunSigs :: term(),
  MbDefs :: term(),
  Types :: term(),
  FunSpecs :: term().
read_attribs(Forms) ->
  lists:foldl(
    fun(_Form = {function, ANNO, Fun, Arity, _}, {Sigs, MbDefs, Types, Specs}) ->
      ?TRACE("Fun: ~p", [_Form]),
      ?DEBUG("~b: ~s/~b", [ANNO, Fun, Arity]),
      {add_fun_sig(ANNO, {Fun, Arity}, Sigs), MbDefs, Types, Specs};

      (_Form = {attribute, ANNO, Modality, {Mailbox, Sigs0}}, {Sigs, MbDefs, Types, Specs})
        when Modality =:= ?M_NEW; Modality =:= ?M_USE ->
        ?TRACE("MbDef: ~p", [_Form]),
        ?DEBUG("~b: -~s ~s with ~p", [ANNO, Modality, Mailbox, Sigs0]),
        {Sigs, add_mb_def(ANNO, Sigs0, Modality, Mailbox, MbDefs), Types, Specs};

      (_Form = {attribute, ANNO, type, {Name, Type, Vars}}, {Sigs, MbDefs, Types, Specs}) ->
        ?TRACE("Type: ~p", [_Form]),
        ?DEBUG("~b: -type ~s", [ANNO, Name]),
        {Sigs, MbDefs, add_type(ANNO, Name, Type, Vars, Types), Specs};

      (_Form = {attribute, ANNO, spec, {Sig = {_Fun, _Arity}, Types0}}, {Sigs, MbDefs, Types, Specs}) ->
        ?TRACE("Spec: ~p", [_Form]),
        ?DEBUG("~b: -spec ~s/~b", [ANNO, _Fun, _Arity]),
        {Sigs, MbDefs, Types, add_fun_spec(ANNO, Sig, Types0, Specs)};

      (_Form, Data) ->
        ?TRACE("Form: ~p", [_Form]),
        ?DEBUG("~b: ~s ~p", [element(2, _Form), element(1, _Form),
          if size(_Form) > 2 -> element(3, _Form); true -> undefined end]),
        Data
    end, {[], [], [], []}, Forms).



analyze_forms(Forms) ->
  lists:foldl(fun analyze_form/2, #form_info{}, Forms).

analyze_form(Form, FormInfo) ->
  ANNO = erl_syntax:get_pos(Form),
  case erl_syntax_lib:analyze_form(Form) of
    {attribute, _} ->
      case erl_syntax_lib:analyze_attribute(Form) of
        Wild = {Name, _}
          when Name =:= type; Name =:= spec; Name =:= ?M_NEW; Name =:= ?M_USE ->
          ?TRACE("Attribute '~p'.", [Wild]),
          add_form_info(Wild, ANNO, FormInfo);
        _ ->
          ?TRACE("Skip attribute '~s'", [erl_prettypr:format(Form)]),
          FormInfo
      end;
    Fun = {function, {_Name, _Arity}} ->
      % Function definition.
      ?TRACE("Function '~s/~b'.", [_Name, _Arity]),
      add_form_info(Fun, ANNO, FormInfo);
    _ ->
      % Other form.
      ?TRACE("Skip form '~s'.", [erl_prettypr:format(Form)]),
      FormInfo
  end.



add_form_info({function, Info}, ANNO, FormInfo = #form_info{fun_refs = FunRefs}) ->
  {_Fun, _Arity} = Info,
%%  FormInfo#form_info{fun_refs = [{ANNO, Info} | FunRefs]};
  FormInfo#form_info{fun_refs = [{function, ANNO, Info} | FunRefs]};
add_form_info({type, Info}, ANNO, FormInfo = #form_info{types = Types}) ->
  {Name, Type, Vars = []} = Info,
%%  FormInfo#form_info{types = [{Name, {ANNO, Type, Vars}} | Types]};
  FormInfo#form_info{types = [{type, ANNO, Info} | Types]};
add_form_info({spec, Info}, ANNO, FormInfo = #form_info{specs = Specs}) ->
  {FunRef = {_Fun, _Arity}, Type} = Info,
%%  FormInfo#form_info{specs = [{FunRef, {ANNO, Type}} | Specs]};
  FormInfo#form_info{specs = [{spec, ANNO, Info} | Specs]};
add_form_info({MbMod, Info}, ANNO, FormInfo = #form_info{mb_defs = MbDefs})
  when MbMod =:= ?M_NEW; MbMod =:= ?M_USE ->
  {MbName, FunRefs} = Info,

  FlatMbDefs =
    if length(FunRefs) =:= 0 ->
      [{mailbox, ANNO, {{MbMod, MbName}, undefined}}];
      true ->
        % Flatten mailbox interface definitions.
        [{mailbox, ANNO, {{MbMod, MbName}, FunRef}} || FunRef <- FunRefs]
    end,
%%  FormInfo#form_info{mb_defs = [{{MbMod, MbName}, {ANNO, FunRefs}} | MbDefs]}.
%%  FormInfo#form_info{mb_defs = [{mailbox, ANNO, {{MbMod, MbName}, FunRefs}} | MbDefs]}.
  FormInfo#form_info{mb_defs = MbDefs ++ FlatMbDefs}.


%%flatten_mb_defs(MbDefs) when is_list(MbDefs) ->
%%  Fun =
%%    fun({mailbox, ANNO, {{MbMod, MbName}, FunRefs}}, MbFunRefs) ->
%%      MbFunRefs ++ [{mailbox, ANNO, {{MbMod, MbName}, FunRef}} || FunRef <- FunRefs]
%%    end,
%%  lists:foldl(Fun, [], MbDefs).

%% TODO: Generate warning for mailbox interface not associated to any function.



-spec get_t_info(erl_syntax:forms()) -> paterl_lib:analysis().
get_t_info(Forms) ->

  % Read raw attributes from AST. The consistency checking of type and function
  % specs w.r.t functions is already performed by the Erlang preprocessor and
  % we do not need to check that.

  {Sigs, MbDefs, Types, Specs} = read_attribs(Forms),
  ?TRACE("Sigs: ~p", [Sigs]),
  ?TRACE("MbDefs: ~p", [MbDefs]),
  ?TRACE("Types: ~p", [Types]),
  ?TRACE("Specs: ~p", [Specs]),

  #form_info{fun_refs = FunRefs, types = Types_, specs = Specs_, mb_defs = MbDefs_} = analyze_forms(Forms),
  ?TRACE("Sigs_: ~p", [FunRefs]),
  ?TRACE("MbDefs_: ~p", [MbDefs_]),
  ?TRACE("Types_: ~p", [Types_]),
  ?TRACE("Specs_: ~p", [Specs_]),

  % Sigs = Specs = list
%%  SigsCtx = #{} = make_sigs_ctx(Sigs),
%%  Specs0 = maps:from_list(Specs),




  SpecsCtx = #{} = make_specs_ctx(Specs_),
%%  MbDefs0 = #{} = make_mb_defs2(MbDefs_),

  % Flatten mbdefs - place in desugaring pass.
%%  MbDefsFlat = flatten_mb_defs(MbDefs_),


  ?DEBUG("SpecsCtx:~n~p~n", [SpecsCtx]),

  case check_fun_specs2(FunRefs, SpecsCtx) of
    Analysis0 = #analysis{status = ok} ->
      % Get mailbox definition map for convenient use for type consistency checking.
      % Check that each function signature is associated to at most one mailbox
      % definition and modality.

      % HERE STARTS MAILBOX TYPE CHECKING.
      case check_unused_mb_defs(MbDefs_) of
        #analysis{status = ok} ->

%%          UsedMbDefs = used_mb_defs(MbDefs_),
%%          ?TRACE("UnusedMbDefs = ~p", [UsedMbDefs]),

          case check_mb_dup_fun_use(MbDefs_, Analysis0) of
            Analysis1 = #analysis{status = ok} ->
              ?TRACE("check_mb_dup_fun_use = ok"),

              MbDefs0 = make_mb_defs3(MbDefs_),
              ?DEBUG("MbDefsCtx:~n~p~n", [MbDefs0]),

              % Check that function signatures used in mailbox definitions are defined.
              case check_mb_fun_refs_defined(MbDefs0, SpecsCtx) of
                #analysis{status = ok} ->
                  % Get unique mailbox names.
                  MbNamesCtx = make_mb_names_ctx(MbDefs),
                  ?DEBUG("MbNamesCtx:~n~p~n", [MbNamesCtx]),
                  MbNamesCtx0 = make_mb_names_ctx2(MbDefs_),
                  ?DEBUG("MbNamesCtx0:~n~p~n", [MbNamesCtx0]),
%%              MbNamesCtx = MbNamesCtx0,


                  % TODO: This can be packaged into an analysis record.
                  TypesCtx = make_types_ctx(Types, MbNamesCtx),
                  TypesCtx0 = make_types_ctx2(Types_, MbNamesCtx0),
                  TypsCtx = TypesCtx0,

                  % Type info record.
                  TInfo =
                    #t_info{
                      types = make_types_ctx2(Types_, MbNamesCtx0),
                      specs = SpecsCtx,
                      mb_names = MbNamesCtx,
                      mb_defs = MbDefs0
                    },
                  X = #analysis{result = TInfo},
                  ?TRACE("Analysis record = ~p", [X]),
                  X;

                Analysis2 = #analysis{status = error} -> % Here we do not need to match against, we can just use the variable Error.
                  % One or more function signatures in mailbox definitions are not
                  % declared as types.
                  Analysis2
              end;
            Analysis1 = #analysis{status = error} ->
              % One or more function signatures are associated with the same mailbox.
              ?TRACE("check_mb_dup_fun_use = error"),
              Analysis1
          end;
        AnalysisX = #analysis{status = error} ->
          AnalysisX
      end;
    Analysis0 = #analysis{status = error} ->
      % One or more function signatures do not have specs.
      ?TRACE("check_fun_specs2 = error"),
      Analysis0
  end.


check_unused_mb_defs(MbDefs) when is_list(MbDefs) ->
  Fun =
    fun({mailbox, _, {{_, _}, _FunRef = {_, _}}}, Analysis) ->
      Analysis;
      ({mailbox, Anno, {{_, MbName}, _FunRef = undefined}}, Analysis) ->
        ?pushWarning(?W_NO__MB_FUN_REF, to_erl_af(Anno, MbName), Analysis)
    end,
  lists:foldl(Fun, #analysis{}, MbDefs).

used_mb_defs(MbDefs) when is_list(MbDefs) ->
  Fun = fun({mailbox, _, {{_, _}, _FunRef = {_, _}}}) -> true; (_) -> false end,
  lists:filter(Fun, MbDefs).


add_fun_sig(ANNO, Sig = {_, _}, Sigs) when is_list(Sigs) ->
  [{ANNO, Sig} | Sigs].

add_mb_def(ANNO, Sigs, Modality, Mailbox, MbSpecs)
  when is_list(Sigs), is_list(MbSpecs) ->
  [{{Modality, Mailbox}, {ANNO, Sigs}} | MbSpecs].

add_type(ANNO, Name, Type, Vars, Types)
  when is_list(Vars), is_list(Types) ->
  [{Name, {ANNO, Type, Vars}} | Types].

add_fun_spec(ANNO, Sig = {_, _}, Types, FunSpecs)
  when is_list(Types), is_list(FunSpecs) ->
  [{Sig, {ANNO, Types}} | FunSpecs].


%% @private Returns the map of function signatures. Function signatures are
%% assumed to be unique.
%% @returns Function signatures.
%%make_sigs_ctx(Sigs) when is_list(Sigs) ->
%%  Fun = fun({ANNO, Sig = {_, _}}, Sigs0) -> Sigs0#{Sig => {ANNO}} end,
%%  Map = lists:foldl(Fun, #{}, Sigs),
%%  if map_size(Map) =:= length(Sigs) -> Map; true -> error(invalid_fun_refs) end.

-doc """
Creates a map of function specs.

Function specs are assumed to be unique and should be enforced by the Erlang
linter.

### Returns
- map of function specs
""".
%%make_specs_ctx(Specs) when is_list(Specs) ->
%%  Fun =
%%    fun({Sig = {_, _}, {ANNO, Spec}}, Ctx) ->
%%      Ctx#{Sig => {spec, ANNO, Spec}}
%%    end,
%%  lists:foldl(Fun, #{}, Specs).

% FINAL.
make_specs_ctx(Specs) when is_list(Specs) ->
  Fun =
    fun({spec, ANNO, {FunRef = {_, _}, Type}}, Ctx) ->
      Ctx#{FunRef => {spec, ANNO, Type}}
    end,
  lists:foldl(Fun, #{}, Specs).


%% @private Returns the map of mailbox names defined.
%% @returns Mailbox names.
make_mb_names_ctx(MbDefs) when is_list(MbDefs) ->
  ?TRACE("make_mb_names_ctx MbDefs = ~p", [MbDefs]),
  lists:foldl(
    fun({{Modality, Name}, {ANNO, _}}, Ctx) ->
%%    fun({mailbox, Anno, {{MbMod, MbName}, FunRef}}, Ctx) ->
%%      maps:update_with(MbName, fun({_, new}) -> {Anno, new}; (_) -> {Anno, MbMod} end, {Anno, MbMod}, Ctx)
      maps:update_with(Name, fun({_, new}) -> {ANNO, new}; (_) -> {ANNO, Modality} end, {ANNO, Modality}, Ctx)
    end,
    #{}, MbDefs).

make_mb_names_ctx2(MbDefs) when is_list(MbDefs) ->
  ?TRACE("make_mb_names_ctx2 MbDefs = ~p", [MbDefs]),
  Fun =
    fun({mailbox, Anno, {{MbMod, MbName}, FunRef}}, Ctx) ->
      maps:update_with(MbName,
        fun({_, new}) -> {Anno, new}; (_) -> {Anno, MbMod} end,
        {Anno, MbMod}, Ctx)
    end,
  lists:foldl(Fun, #{}, MbDefs).


make_types_ctx(Types, MbNames = #{}) when is_list(Types) ->
%%  Mailboxes = lists:map(fun({_, _, Name}) -> Name end, maps:values(MbDefs)),
  lists:foldl(
    fun({Name, {ANNO, Type, Vars}}, Ctx) ->
      case maps:is_key(Name, MbNames) of
%%      case maps:take(Name, MbNames) of %TODO: Optimisation if we use take!
        true ->

          % Type defines a mailbox.
          Ctx#{Name => {?T_MBOX, ANNO, Type, Vars}};
        false ->

          % Type does not define a mailbox.
          Ctx#{Name => {?T_TYPE, ANNO, Type, Vars}}
      end
    end,
    #{}, Types).

%%{Name, Type, Vars = []} = Info,
%%%%  FormInfo#form_info{types = [{Name, {ANNO, Type, Vars}} | Types]};
%%FormInfo#form_info{types = [{type, ANNO, Info} | Types]};

make_types_ctx2(Types, MbNames) when is_list(Types), is_map(MbNames) ->
  Fun =
    fun({type, Anno, {Name, Type, Vars = []}}, Ctx) ->
      case maps:is_key(Name, MbNames) of
        true ->
          % User-defined mailbox interface type.
          Ctx#{Name => {?T_MBOX, Anno, Type, Vars}};
        false ->
          % User-defined data type.
          Ctx#{Name => {?T_TYPE, Anno, Type, Vars}}
      end
    end,
  lists:foldl(Fun, #{}, Types).


%%% ----------------------------------------------------------------------------
%%% Syntactic checking functions.
%%% ----------------------------------------------------------------------------

% 1. Check that the new and use attributes have the correct format.
% TODO: Maybe move this to the other module concerned with syntactic checking
% TODO: and leave this module only for semantics/consistency checks.

-doc """
Checks that the -new and -use wild attributes have the correct format.
- `Forms` is the list of forms.

**Returns**
The `{compiler_internal,term()}` option is forwarded to the Erlang token
scanner, see [`{compiler_internal,term()}`](`m:erl_scan#compiler_interal`).

- `{ok, Result, []}` if the syntactic check succeeds, `{error, Errors, []}`
otherwise.
""".
%%-spec check_mb_specs_syntactic(Forms) -> {ok, Forms, []} | error()
%%  when
%%  Forms :: erl_syntax:forms().
%%-spec check_mb_specs_syntactic(erl_syntax:forms()) -> paterl_lib:analysis().
%%check_mb_specs_syntactic(Forms) when is_list(Forms) ->
%%  AttribCheckFun =
%%%%    fun(Attrib = {attribute, _, Modality, {Mailbox, Sigs}}, Analysis)
%%  fun(Attrib = {attribute, ANNO, Modality, Value}, Analysis)
%%    when Modality =:= ?M_NEW; Modality =:= ?M_USE ->
%%
%%    case Value of
%%      {Mailbox, Sigs} when is_atom(Mailbox), is_list(Sigs) ->
%%        % Well-structured mailbox spec attribute. Check validity of signature
%%        % definitions in the spec.
%%        % Other signature forms, such as atom/atom are checked by the Erlang
%%        % preprocessor.
%%
%%
%%        FunSigCheckFun =
%%          fun(Sig = {Name, Arity}, Analysis0) ->
%%            ?TRACE("Processing sig: ~p", [Sig]),
%%            case is_fun_sig(Sig) of
%%              true ->
%%                % Valid function signature.
%%                Analysis0;
%%              false ->
%%                % Invalid function signature.
%%                Node = to_erl_af(ANNO, Sig),
%%%%                  erl_syntax:revert(
%%%%                    erl_syntax:set_pos(
%%%%                      erl_syntax:arity_qualifier(erl_syntax:atom(Name), erl_syntax:integer(Arity)),
%%%%                      erl_syntax:get_pos(Attrib)
%%%%                      )),
%%                ?pushError(?E_MB_SIG_BAD, Node, Analysis0)
%%            end
%%          end,
%%
%%        % Check validity of function signature definitions.
%%        lists:foldl(FunSigCheckFun, Analysis, Sigs);
%%
%%%%          true ->
%%%%            % Invalid mailbox spec attribute.
%%%%            ?pushError(?E_MB_SPEC_BAD, Attrib, Analysis)
%%%%        end;
%%      _ ->
%%        % Invalid mailbox spec attribute.
%%        ?pushError(?E__BAD_MB_DEF, Attrib, Analysis)
%%    end;
%%
%%    (_, Analysis) ->
%%      % Other attributes.
%%      Analysis
%%  end,
%%
%%%%  AttributeCheckFun =
%%%%    fun(Node = {attribute, _, Modality, {Mailbox, Sigs}}, Error)
%%%%      when
%%%%      Modality =:= ?M_NEW, is_atom(Mailbox), is_list(Sigs);
%%%%      Modality =:= ?M_USE, is_atom(Mailbox), is_list(Sigs) ->
%%%%
%%%%      SigsCheckFun =
%%%%        fun({Name, Arity}, Error) when is_atom(Name), Arity >= 0, Arity =< 255 ->
%%%%          Error;
%%%%          (_, Error = #analysis{}) ->
%%%%
%%%%            % Invalid function signature. Other signature forms, such as
%%%%            % atom/atom are checked by the Erlang preprocessor.
%%%%            ?pushError(?E_MB_SIG_BAD, Node, Error)
%%%%        end,
%%%%      lists:foldl(SigsCheckFun, Error, Sigs);
%%%%
%%%%      (Node = {attribute, _, Modality, _}, Error)
%%%%        when Modality =:= ?M_NEW; Modality =:= ?M_USE ->
%%%%
%%%%        % Invalid mailbox spec.
%%%%        ?pushError(?E_MB_SPEC_BAD, Node, Error);
%%%%      (_, Error) ->
%%%%
%%%%        % Valid attribute.
%%%%        Error
%%%%    end,
%%
%%  % Check validity of mailbox definition attributes.
%%  lists:foldl(AttribCheckFun, #analysis{}, Forms).

is_fun_sig({Name, Arity}) when is_atom(Name), Arity >= 0, Arity =< 255 ->
  true;
is_fun_sig(_) ->
  false.

%%is_fun_sigs([]) ->
%%  true;
%%is_fun_sigs([{Name, Arity} | FunSigs])
%%  when
%%  is_atom(Name), Arity >= 0, Arity =< 255 ->
%%  is_fun_sigs(FunSigs);
%%is_fun_sigs(_) ->
%%  false.

%%is_fun_sigs([FunSig | FunSigs]) ->
%%  case is_fun_sig(FunSig) of
%%    true ->
%%      is_fun_sigs(FunSigs);
%%    false ->
%%      false
%%  end.


any(_, []) ->
  false;
any(Pred, [H | T]) when is_function(Pred, 1) ->
  case Pred(H) of
    false ->
      any(Pred, T);
    true ->
      true
  end.

all(_, []) ->
  true;
all(Pred, [H | T]) when is_function(Pred, 1) ->
  case Pred(H) of
    true ->
      all(Pred, T);
    false ->
      false
  end.


%%% ----------------------------------------------------------------------------
%%% Type consistency checking functions.
%%% ----------------------------------------------------------------------------

%% Returns the mailbox spec map relating function signatures to mailbox
%% modalities and names.
%% Also checks that each function signature is associated with at most one
%% mailbox name in either the new or use modality.
%% @returns {ok, MbSpecs, Warnings} or {error, Errors, Warning}. Warnings is
%% always the empty list for this call.
-spec make_mb_defs(list()) -> paterl_lib:analysis().
make_mb_defs(MbDefs) when is_list(MbDefs) ->
  MbDefFun =
    fun({{Modality, Mailbox}, {ANNO, Sigs}}, Analysis = #analysis{}) ->
      UniqueCheckFun =
        fun(Sig = {_, _}, Analysis0 = #analysis{result = MbDefs = #{}}) ->
          case maps:is_key(Sig, MbDefs) of
            true ->
              % Function signature already associated with other mailbox type.
              Node = to_erl_af(ANNO, Sig),
              ?pushError(?E_MB_SIG_NOT_UNIQUE, Node, Analysis0);
            false ->
              % Associate function signature with mailbox type.
              Analysis0#analysis{result = MbDefs#{Sig => {Modality, ANNO, Mailbox}}}
          end
        end,

      % Check that mailbox definition-function signature association is unique.
      % Will be relaxed later due to the -new and -use issue.
      lists:foldl(UniqueCheckFun, Analysis, Sigs)
    end,

  % Convert the list of mailbox definitions to a map, checking mailbox
  % definition-function signature association is unique.
  lists:foldl(MbDefFun, #analysis{result = #{}}, MbDefs).


%%make_mb_defs2(MbDefs) when is_list(MbDefs) ->
%%  Fun =
%%    fun({mailbox, ANNO, {{MbMod, MbName}, FunRefs}}, Ctx) ->
%%      Fun0 =
%%        fun(FunRef = {_, _}, Ctx) ->
%%          Ctx#{FunRef => {MbMod, ANNO, MbName}}
%%        end,
%%      lists:foldl(Fun0, Ctx, FunRefs)
%%    end,
%%
%%  % Map mailbox interface definition list to map. No mailbox interface-function
%%  % reference definition are checked for uniqueness.
%%  lists:foldl(Fun, #{}, MbDefs).

make_mb_defs3(MbDefs) when is_list(MbDefs) ->
  Fun =
    fun({mailbox, Anno, {{MbMod, MbName}, FunRef = {_, _}}}, Ctx) ->
      Ctx#{FunRef => {MbMod, Anno, MbName}};
      ({mailbox, _, {{_, _}, _FunRef = undefined}}, Ctx) ->
        Ctx
    end,
  lists:foldl(Fun, #{}, MbDefs).


% Test:
% -new({mb, [f/0]}).
% -new({mb0, [f/0]}).
% This should work but it does not.

%%check_unique_mb_defs(MbDefs, Analysis)
%%  when is_list(MbDefs) ->
%%  Fun =
%%    fun({mailbox, ANNO, {{MbMod, MbName}, FunRefs}}, {MbFunRefs, Analysis}) ->
%%      ?TRACE("Check mailbox definition -~s(~s).", [MbMod, MbName]),
%%%%      Fun2 =
%%%%        fun(FunRef = {_FunName, _Arity}, {UniqueFunRefs, Analysis}) ->
%%%%          ?TRACE("Check fun reference ~s/~b.", [_FunName, _Arity]),
%%%%          case sets:is_element({MbName, FunRef}, UniqueFunRefs) of
%%%%            true ->
%%%%              ?ERROR("FunRef ~p already exists!", [{MbName, FunRef}]),
%%%%
%%%%              % Fun ref x already associated with mailbox X in use modality.
%%%%
%%%%              % Function signature already associated with other mailbox type.
%%%%              % Fun ref associated with the same mailbox with the same modality
%%%%              % ro with the same mailbox under a different modality.
%%%%              Node = to_erl_af(ANNO, FunRef),
%%%%              {UniqueFunRefs, ?pushError(?E_MB_SIG_NOT_UNIQUE, Node, Analysis)};
%%%%            false ->
%%%%              {sets:add_element({MbName, FunRef}, UniqueFunRefs), Analysis}
%%%%          end
%%%%        end,
%%%%      lists:foldl(Fun2, {UniqueFunRefs, Analysis}, FunRefs)
%%      case get_mb_dup_fun_use(MbName, FunRefs, MbFunRefs) of
%%        {[], MbFunRefs0} ->
%%          % No duplicate mailbox interface-fun reference uses.
%%          {MbFunRefs0, Analysis};
%%        {[_ | _], MbFunRefs} ->
%%          % Duplicate mailbox interface-fun uses. Duplicate use of a mailbox (in
%%          % any modality) with the same fun reference is not permitted.
%%          ?ERROR("Duplicate functions found at ~p", [MbName]),
%%
%%
%%          {MbFunRefs, Analysis}
%%      end
%%    end,
%%  lists:foldl(Fun, {sets:new(), Analysis}, MbDefs).

%%get_mb_dup_fun_use(MbName, FunRefs, MbFunRefs) ->
%%  Fun2 =
%%    fun(FunRef = {_FunName, _Arity}, {DupFunRefs, MbFunRefs0}) ->
%%%%      erl_syntax:implicit_fun(erl_syntax:atom(_FunName), erl_syntax:integer(_Arity)),
%%%%      ?TRACE("Check fun reference ~s/~b.", [_FunName, _Arity]),
%%      % Create mailbox interface name-fun reference association.
%%      MbFunRef = {MbName, FunRef},
%%      case sets:is_element(MbFunRef, MbFunRefs0) of
%%        true ->
%%          % Mailbox interface already used with fun reference. The duplicate use
%%          % does not distinguish between -new and -use modalities, treating them
%%          % as a duplication.
%%%%          ?ERROR("Fun ref ~p already associated with mailbox interface definition ~s.", [FunRef, MbName]),
%%          {[FunRef | DupFunRefs], MbFunRefs0};
%%        false ->
%%          % Mailbox interface not yet used with fun reference. Add it to set.
%%          {DupFunRefs, sets:add_element(MbFunRef, MbFunRefs0)}
%%      end
%%    end,
%%  lists:foldl(Fun2, {_DupFunRefs = [], MbFunRefs}, FunRefs).


%% FINAL.
check_mb_dup_fun_use(MbDefs, Analysis = #analysis{}) when is_list(MbDefs) ->
  Fun =
    fun({mailbox, Anno, {{_, MbName}, FunRef = {_, _}}}, {MbFunRefs, Analysis}) ->
      % Mailbox interface-fun reference association.
      MbFunRef = {MbName, FunRef},
      case lists:member(MbFunRef, MbFunRefs) of
        true ->
          % Mailbox interface already used with another fun reference.
          % Duplicate usage of mailbox interfaces by fun references does not
          % distinguish between -new and -use modalities, treating them
          % both as duplications.
          Node = paterl_syntax:fun_reference(FunRef, Anno),

          ?ERROR("Duplicate fun reference '~s' use in mailbox interface '~s'", [
            erl_prettypr:format(Node), MbName
          ]),
          {MbFunRefs, ?pushError(?E_MB_SIG_NOT_UNIQUE, Node, Analysis)};
        false ->
          % Mailbox interface not yet used by a fun reference.
          {[MbFunRef | MbFunRefs], Analysis}
      end;
      ({mailbox, _, {{_, _MbName}, _FunRef = undefined}}, Acc = {_, _}) ->
        ?WARN("Skip unused mailbox interface '~s'.", [_MbName]),
        Acc
    end,
  {_, Analysis0} = lists:foldl(Fun, {_MbFunRefs = [], Analysis}, MbDefs),
  Analysis0.


%% @private Checks that the function signatures in mailbox definitions are
%% defined.
%% returns {ok, Warnings} if all signatures are defined, otherwise
%% {error, Warnings, Errors}. Warnings is always the empty list.
-spec check_mb_fun_refs_defined(#{}, #{}) -> paterl_lib:analysis().

% FINAL.
check_mb_fun_refs_defined(MbDefs, FunRefs)
  when is_map(MbDefs), is_map(FunRefs) ->
  Fun =
    fun(FunRef = {_, _}, {_, Anno, _MbName}, Analysis) ->
      case maps:is_key(FunRef, FunRefs) of
        true ->
          % Defined fun reference in mailbox interface definition.
          Analysis;
        false ->
          % Undefined fun reference in mailbox interface definition.
          Node = paterl_syntax:fun_reference(FunRef, Anno),
          ?TRACE("Undefined fun reference '~s' in mailbox interface '~s'.", [
            erl_prettypr:format(Node), _MbName
          ]),
          ?pushError(?E_UNDEF__FUN_REF, Node, Analysis)
      end
    end,

  % Check that fun references in mailbox interface definitions are defined.
  maps:fold(Fun, #analysis{}, MbDefs).


%% @private Checks that mailbox names have a corresponding type defined.
%% returns `{ok, Warnings}` if all mailbox names have a corresponding type
%% defined, otherwise `{error, Errors, Warnings}`. Warnings is always the empty
%% list.
-spec check_mb_types_defined(#{}, #{}) -> paterl_lib:analysis().
check_mb_types_defined(MbNames = #{}, Types = #{}) ->
  MbNameCheckFun =
    fun(MbName, {ANNO, _}, Analysis = #analysis{}) ->
      ?TRACE("Checking MbName = ~p", [MbName]),
      case maps:is_key(MbName, Types) of
        true ->
          % Mailbox name defined as user-defined type.
          Analysis;
        false ->
          % Undefined mailbox user-defined type.
          Node = to_erl_af(ANNO, MbName),
          ?pushError(?E_UNDEF__MB_TYPE, Node, Analysis)
      end
    end,

  % Check that mailbox names are defined as user-defined types.
  maps:fold(MbNameCheckFun, #analysis{}, MbNames).


%% TODO: Document this better.
%% TODO: This must be updated to account for the fact that we can have a list of mailboxes that where some are new and some are used?.
%% TODO: Maybe one way to deal with it is to remove lists from mailbox definitions

%%HERE - Rerun newness unit test to confirm
-spec check_mb_new(#{}) -> paterl_lib:analysis().
check_mb_new(MbNames) when is_map(MbNames) ->
  ?TRACE("Checking for newness"),
  MbNameNewCheckFun =
    fun(_, {_, ?M_NEW}, Analysis = #analysis{}) ->
      % Mailbox name is new.
      Analysis;
      (MbName, {ANNO, ?M_USE}, Error) ->
        % Mailbox name is use.
        ?pushError(?E_NO__MB_NEW, to_erl_af(ANNO, MbName), Error)
    end,

  % Check that mailbox name is annotated with new and is not used without new.
  maps:fold(MbNameNewCheckFun, #analysis{}, MbNames).


-spec check_mb_types_valid(#{}) -> paterl_lib:analysis().
check_mb_types_valid(Types = #{}) ->
  MbTypeCheckFun =
    fun(Name, {?T_MBOX, ANNO, Type, _TypeVars}, Analysis) ->
      ?TRACE("Check user-defined mailbox interface type '~s() :: ~s'.", [Name, erl_prettypr:format(Type)]),
      % Check user-defined mailbox interface type.

      % Do not check for nor stop in case of errors: these are handled by
      % caller functions.
%%      case check_msg_type_valid(Type, Types, Analysis, false) of
%%        Analysis0 = #analysis{status = error} ->
%%          % User-defined message type invalid.
%%          Analysis0;
%%
%%        Analysis0 = #analysis{status = ok, result = true} ->
%%%%        {true, Error0} ->
%%
%%          ?TRACE("Pid present in type defintion."),
%%          % PID present in mailbox definition.
%%          Analysis0;
%%        Analysis0 = #analysis{status = ok, result = false} ->
%%%%        {false, Error0} ->
%%
%%          % PID missing.
%%          ?TRACE("Pid MISSING ~p", [Analysis0]),
%%
%%%%            Node = revert(set_pos(atom(Name), ANNO)),
%%          Node = to_erl_af(ANNO, Name),
%%          ?pushWarning(?W_MB_NO_PID, Node, Analysis0)
%%      end;
      Analysis0 = paterl_lib:reset_status(Analysis),
      Anal = check_msg_type_valid3(Name, ANNO, Type, Types, Analysis0),
      ?TRACE("--- Anal: ~p", [Anal]),
      Anal;
      (_, {?T_TYPE, _, _, _}, Analysis) ->
        % Skip user-defined non-mailbox interface types.
        Analysis
    end,

  % Check validity of mailbox types definition including missing PIDs.
  Analysis = maps:fold(MbTypeCheckFun, #analysis{}, Types),


  % Result is internal and should not be visible to callers. Only status is
  % relevant.
%%  Analysis#analysis{result = undefined}.
  paterl_lib:reset_result(paterl_lib:set_status(Analysis)).


check_msg_type_valid3(MbName, ANNO, MsgType, Types, Analysis) ->

%%  case check_msg_type_valid2(MsgType, Types, Analysis#analysis{result = false}) of
%%    Analysis0 = #analysis{status = error} ->
%%      % User-defined message type invalid.
%%      ?ERROR("Invalid user-defined mailbox interface type '~s'.", [MbName]),
%%      Analysis0;
%%    Analysis0 = #analysis{status = ok, result = HadPidType} ->
%%      % User-defined message type valid and the extra build-in Pid type is
%%      % included in the message type definition.
%%      if HadPidType -> Analysis0;
%%        true ->
%%          ?WARN("Missing built-in 'pid()' type."),
%%          ?pushWarning(?W_MB_NO_PID, to_erl_af(ANNO, MbName), Analysis0)
%%      end
%%  end.
  case check_msg_type_valid2(MsgType, Types, Analysis#analysis{result = false}) of
    Analysis0 = #analysis{result = false} ->
      ?WARN("Missing built-in 'pid()' type."),
      ?pushWarning(?W_NO__PID, to_erl_af(ANNO, MbName), Analysis0);
    Analysis0 ->
      Analysis0
  end.

check_msg_type_valid2(_Type = {type, _ANNO, pid, _}, #{}, Analysis) ->
  % Built-in pid type found.
  ?TRACE("Check built-in type '~s'.", [erl_prettypr:format(_Type)]),
  Analysis#analysis{result = _HasPidType = true};
check_msg_type_valid2(_Type = {type, _ANNO, tuple, [Tag | Elems]}, Types = #{}, Analysis) ->
  % Inline message type.
  ?TRACE("Check inline message type '~s'.", [erl_prettypr:format(_Type)]),

  % Check that first element is a valid tag.
  case check_valid_tag(Tag, Analysis) of
    #analysis{status = ok} ->
      % Check that rest of tuple elements are valid types. Only primitive,
      % user-defined message, or user-defined mailbox interface types are
      % permitted as elements.

      ?TRACE("After validating the tag ~p", [Tag]),
      check_valid_tuple_elems2(Elems, Types, Analysis);


    Analysis0 = #analysis{status = error} ->
      ?TRACE("After invalidating the tag ~p", [Tag]),
      % First element not a valid tag.
      Analysis0
  end;
check_msg_type_valid2(_Type = {user_type, _ANNO, Name, _TypeVars}, Types = #{}, Analysis) ->
  % User-defined message type. Only user-defined message types are permitted in
  % mailbox interface definitions.
  ?TRACE("Check user-defined message type '~s'.", [erl_prettypr:format(_Type)]),


  case maps:get(Name, Types, undefined) of
    {?T_TYPE, _, Type0, _} ->
      % User-defined type is a message type.
      ?TRACE("Valid user-defined message type '~s'.", [erl_prettypr:format(_Type)]),
      ?TRACE(">> Type: ~p", [Name]),

%%      check_msg_type_valid3(MbName, ANNO, MsgType, Types, Analysis)
      check_msg_type_valid2(Type0, Types, Analysis);
    {?T_MBOX, _, Type0, _} ->
      % User-defined type is a mailbox interface type.
      ?ERROR("Invalid user-defined message type '~s'.", [erl_prettypr:format(_Type)]),
      %TODO: Unexpected mailbox interface type error.
%%      ?pushError(?E_MB_MSG_ELEM_TYPE_BAD, Elem, Analysis),
      ?pushError(?E_BAD__MSG_TYPE, _Type, Analysis);

%%      error(lists:flatten(io_lib:format("'~s' is not normal type.", [erl_prettypr:format(_Type)])));
    undefined ->
      ?ERROR("Undefined type '~s'.", [erl_prettypr:format(_Type)]),
      % Undefined types are ruled out by the Erlang preprocessor/liner. This
      % case should never arise if erl_lint was used in prior passes.
      ?pushError(?E__UNDEF_TYPE, _Type, Analysis)
%%      error("Undefined types are ruled out by the preprocessor/linter; use epp or erl_lint")
  end;
check_msg_type_valid2(_Type = {type, _ANNO, union, MsgTypes}, Types = #{}, Analysis) ->
  % Inline message types or user-defined message types. User-defined mailbox
  % interface types are not permitted in type unions.
  ?TRACE("Check union type '~s'.", [erl_prettypr:format(_Type)]),
  check_msgs_types_valid2(MsgTypes, Types, Analysis);

check_msg_type_valid2(Type, _, Analysis) ->
  % Invalid type.
  ?ERROR("Invalid type '~s'", [erl_prettypr:format(Type)]),
  ?pushError(?E_BAD__MSG_TYPE, Type, Analysis).



check_msgs_types_valid2([], _Types = #{}, Analysis) ->
  Analysis;
check_msgs_types_valid2([MsgType | MsgTypes], Types = #{}, Analysis) ->
  Analysis0 = check_msg_type_valid2(MsgType, Types, Analysis),
  check_msgs_types_valid2(MsgTypes, Types, Analysis0).


check_valid_tuple_elems2([], #{}, Analysis) ->
  Analysis;
check_valid_tuple_elems2([_Type = {type, _ANNO, integer, _} | Elems], Types = #{}, Analysis) ->
  % Built-in primitive type.
  ?TRACE("Check built-in primitive type '~s'.", [erl_prettypr:format(_Type)]),
  check_valid_tuple_elems2(Elems, Types, Analysis);
check_valid_tuple_elems2([_Type = {user_type, _ANNO, Name, _} | Elems], Types = #{}, Analysis) ->
  % User-defined mailbox interface type. Only primitive and user-defined mailbox
  % interface types are permitted in message tuple elements.
  ?TRACE("Check user-defined mailbox interface type '~s'.", [erl_prettypr:format(_Type)]),
  ?TRACE(">> Types map: ~p", [Types]),


  case maps:get(Name, Types, undefined) of
    {?T_MBOX, _, Type0, _Vars} ->
      % User-defined type is a mailbox interface type.
      ?TRACE("Valid user-defined mailbox interface type '~s'.", [erl_prettypr:format(_Type)]),
      check_valid_tuple_elems2(Elems, Types, Analysis);
    {?T_TYPE, _, Type0, _Vars} ->
      ?ERROR("Invalid user-defined mailbox interface type '~s'.", [erl_prettypr:format(_Type)]),
      % User-defined type is a normal type.
      % TODO: Unexpected type.
      ?pushError(?E_BAD__MB_TYPE, _Type, Analysis);
%%      error(lists:flatten(io_lib:format("'~s' is not a mailbox interface type.", [erl_prettypr:format(_Type)])));
    undefined ->
      ?ERROR("Undefined type '~s'.", [erl_prettypr:format(_Type)]),
      % Undefined types are ruled out by the Erlang preprocessor/liner. This
      % case should never arise if erl_lint was used in prior passes.
      ?pushError(?E__UNDEF_TYPE, _Type, Analysis)
%%      error("Undefined types are ruled out by the preprocessor/linter; use epp or erl_lint")
  end;


check_valid_tuple_elems2([Elem | Elems], Types = #{}, Analysis) ->
  ?TRACE("Invalid type '~s' in message.", [erl_prettypr:format(Elem)]),
  Analysis0 = ?pushError(?E_BAD__MSG_ELEM_TYPE, Elem, Analysis),
  check_valid_tuple_elems2(Elems, Types, Analysis0).


%%-spec check_msg_type_valid(Type, Types, Analysis, HasPid) -> paterl_lib:analysis()
%%  when
%%  Type :: {type, anno(), atom(), term()},
%%  Types :: map(),
%%  Analysis :: paterl_lib:analysis(),
%%  HasPid :: boolean().
%%check_msg_type_valid(_Type = {type, _ANNO, pid, _}, #{}, Analysis, _) ->
%%  ?TRACE("Type: ~p", [_Type]),
%%  Analysis#analysis{result = true};
%%%%  {true, Error};
%%
%%check_msg_type_valid(_Type = {type, _ANNO, tuple, [Node]}, #{}, Analysis, HasPid) ->
%%  ?TRACE("Type: ~p", [_Type]),
%%  % Message type with tag and no payload.
%%
%%%%  case is_valid_tag(Elem) of
%%%%    true ->
%%%%      ?TRACE("~p is a valid tag", [Elem]),
%%%%      {HasPid, Error};
%%%%    false ->
%%%%      ?TRACE("~p is NOT a valid tag", [Elem]),
%%%%      {HasPid, ?pushError(?E_MB_MSG_TAG_BAD, Elem, Error)}
%%%%  end;
%%  Analysis0 = check_valid_tag(Node, Analysis),
%%  Analysis0#analysis{result = HasPid};
%%%%  {HasPid, check_valid_tag(Node, Analysis)};
%%
%%
%%check_msg_type_valid(_Type = {type, _, tuple, [Elem | Elems]}, Types = #{}, Analysis, HasPid) ->
%%  ?TRACE("Type: ~p", [_Type]),
%%  % Message type with tag and payload.
%%
%%
%%%%  case is_valid_tag(Elem) of
%%%%    true ->
%%%%      ?TRACE("~p is a valid tag", [Elem]),
%%%%
%%%%      case is_valid_payload(Elems, Types) of
%%%%        true ->
%%%%          ?TRACE("~p is a valid payload", [Elems]),
%%%%          {HasPid, Error};
%%%%        false ->
%%%%          ?TRACE("~p is NOT a valid payload", [Elems]),
%%%%          {HasPid, Error}
%%%%      end;
%%%%    false ->
%%%%      ?TRACE("~p is NOT a valid tag", [Elem]),
%%%%      {false, Error}
%%%%  end;
%%
%%  case check_valid_tag(Elem, Analysis) of
%%    #analysis{status = ok} ->
%%      Analysis0 = check_valid_tuple_elems(Elems, Types, Analysis),
%%      Analysis0#analysis{result = HasPid};
%%%%      {HasPid, check_valid_tuple_nodes(Nodes, Types, Analysis)};
%%    Analysis0 = #analysis{status = error} ->
%%      Analysis0#analysis{result = HasPid}
%%%%      {HasPid, Error0}
%%  end;
%%check_msg_type_valid(_Type = {user_type, _ANNO, Type, _TypeVars}, Types = #{}, Analysis, HasPid) ->
%%  ?TRACE("USER Type: ~p", [_Type]),
%%
%%  % Query types map.
%%%%  case maps:is_key(Type, Types) of
%%%%    true ->
%%%%      {HasPid, Error};
%%%%    false ->
%%%%      {HasPid, Error}
%%%%  end;
%%  case maps:get(Type, Types, undefined) of
%%    {_, _, Type0, _} ->
%%
%%      ?TRACE("The found usertype is ~p", [Type0]),
%%
%%%%      {A, B} = check_msg_type_valid(Type0, Types, Error, HasPid),
%%%%      ?TRACE("HasPid in user type? ~p", [A]),
%%
%%      check_msg_type_valid(Type0, Types, Analysis, HasPid);
%%    undefined ->
%%
%%      % TODO: maybe use an assert.
%%      % This case is handled by the Erlang preprocessor and should never happen.
%%      Analysis#analysis{result = HasPid}
%%%%      {HasPid, Analysis}
%%  end;
%%check_msg_type_valid(_Type = {type, _ANNO, union, MsgTypes}, Types = #{}, Analysis, HasPid) ->
%%  ?TRACE("Type: ~p", [_Type]),
%%  check_msgs_types_valid(MsgTypes, Types, Analysis, HasPid);
%%check_msg_type_valid(Term, _Types = #{}, Analysis, HasPid) ->
%%  ?ERROR("Type: ~p", [Term]),
%%  % Type not permitted.
%%  ?pushError(?E_MB_MSG_ELEM_TYPE_BAD, Term, Analysis).
%%%%  Analysis#analysis{result = HasPid}.
%%%%  {HasPid, Analysis}.


%%-spec check_msgs_types_valid(MsgTypes, Types, Analysis, HasPid) ->
%%  paterl_lib:analysis()
%%  when
%%  MsgTypes :: list(),
%%  Types :: map(),
%%  Analysis :: paterl_lib:analysis(),
%%  HasPid :: boolean().
%%check_msgs_types_valid([], _Types = #{}, Analysis, HasPid) ->
%%  Analysis#analysis{result = HasPid};
%%%%  {HasPid, Analysis};
%%check_msgs_types_valid([MsgType | MsgTypes], Types = #{}, Analysis, HasPid) ->
%%  Analysis0 = #analysis{result = HasPid0} =
%%    check_msg_type_valid(MsgType, Types, Analysis, HasPid),
%%  check_msgs_types_valid(MsgTypes, Types, Analysis0, HasPid0).
%%%%  {HasPid0, Error0} = check_msg_type_valid(Elem, Types, Analysis, HasPid),
%%%%  check_msgs_types_valid(Elems, Types, Error0, HasPid0).


%% Checks that the specified term is an atom.
-spec check_valid_tag(term(), paterl_lib:analysis()) -> paterl_lib:analysis().
check_valid_tag(_Tag = {atom, _, _}, Analysis) ->
  ?TRACE("Valid '~s' tag.", [erl_prettypr:format(_Tag)]),
  Analysis;
check_valid_tag(Term, Analysis) ->
  ?TRACE("Invalid '~s' tag.", [erl_prettypr:format(Term)]),
  ?pushError(?E_BAD__MSG_TAG, Term, Analysis).


%%-spec check_valid_tuple_elems(Elems, Types, Analysis) -> paterl_lib:analysis()
%%  when
%%  Elems :: list(),
%%  Types :: map(),
%%  Analysis :: paterl_lib:analysis().
%%check_valid_tuple_elems([], #{}, Analysis) ->
%%  Analysis;
%%check_valid_tuple_elems([{type, _ANNO, integer, _} | Elems], Types = #{}, Analysis) ->
%%  % Type is built-in.
%%  check_valid_tuple_elems(Elems, Types, Analysis);
%%check_valid_tuple_elems([Node = {user_type, _ANNO, Type, _} | Elems], Types = #{}, Analysis) ->
%%  % User defined-type name must refer to a mailbox type definition. Check that
%%  % the mailbox type is defined as a user-defined type in its turn.
%%  %% TODO: Check whether this consistency check that checks the types of
%%  %% individual tuple elements is required. It could be that the Erlang
%%  %% preprocessor also does this, in which case we can remove the map:is_key
%%  %% check.
%%
%%  % TODO: Checking that the type of the user-defined type in the tuple is not
%%  % TODO: enough: it may exist but be an atom, say. It must be a message, so
%%  % TODO: we must check the type of that mailbox type as well.
%%  % HOW: 1. get Type from Types, 2. Check the obtained type.
%%%%  check_msg_type_valid(Type, Types, Analysis, false)..maybe! See on monday!
%%%%  case maps:is_key(Type, Types) of
%%%%    true ->
%%%%      % User-defined mailbox type definition exists. Check rest.
%%%%
%%%%      check_valid_tuple_elems(Elems, Types, Analysis);
%%%%    false ->
%%%%      % Non-existent user-defined mailbox type definition. This never occurs
%%%%      % because the Erlang preprocessor ensures types are valid.
%%%%
%%%%      % TODO: This should never occur because the Erlang preprocessor ensures that types are valid.
%%%%      check_valid_tuple_elems(Elems, Types, ?pushError(?E_MB_TYPE_UNDEF, Node, Analysis))
%%%%  end;
%%
%%  case is_map_key(Type, Types) of
%%    true ->
%%      % User-defined mailbox interface type exists. Check rest of tuple
%%      % elements.
%%      ?DEBUG("User-defined mailbox interface type ~p found as type.", [Type]),
%%      ?DEBUG("Checking rest of elements"),
%%
%%      ?DEBUG("~n~n~n~nTypes table: ~p", [Types]),
%%      {?T_TYPE, _, TypeDef, _} = maps:get(Type, Types),
%%      ?DEBUG("User-defined message type is: ~p", [TypeDef]),
%%      Analysis0 = check_msg_type_valid(TypeDef, Types, Analysis, false),
%%      ?DEBUG("Answer is: ~p", [Analysis0]),
%%
%%      check_valid_tuple_elems(Elems, Types, Analysis0);
%%
%%    false ->
%%      error("Undefined types are ruled out by the preprocessor or linter; use epp or erl_lint"),
%%      check_valid_tuple_elems(Elems, Types, ?pushError(?E_MB_TYPE_UNDEF, Node, Analysis))
%%  end;
%%
%%check_valid_tuple_elems([Elem | Elems], Types = #{}, Analysis) ->
%%  Analysis0 = ?pushError(?E_MB_MSG_ELEM_TYPE_BAD, Elem, Analysis),
%%  check_valid_tuple_elems(Elems, Types, Analysis0).


%% @private Checks that function signatures have a corresponding function spec
%% defined.
%% returns `{ok, Warnings}` if all function signatures have a corresponding
%% spec defined, otherwise `{error, Errors, Warnings}`. Warnings is always the
%% empty list.
%%-spec check_sigs_have_specs(map(), map()) -> {ok, []} | {ok, term(), term()}.
check_fun_specs(FunSigs = #{}, FunSpecs = #{}) ->
  FunSpecCheckFun =
    fun(FunSig = {_, _}, {ANNO}, Analysis) ->
      if is_map_key(FunSig, FunSpecs) ->
        % Function signature has corresponding function spec.
        Analysis;
        true ->
          % Non-existent function spec.
          Node = to_erl_af(ANNO, FunSig),
          ?pushError(?E_UNDEF__FUN_SPEC, Node, Analysis)
      end

%%      case maps:is_key(FunSig, FunSpecs) of
%%        true ->
%%          Analysis;
%%        false ->
%%          Node = to_erl_af(ANNO, FunSig),
%%          ?pushError(?E_MB_SIG_TYPE_UNDEF, Node, Analysis)
%%      end
    end,

  % Check that function signatures have a corresponding function spec defined.
  maps:fold(FunSpecCheckFun, #analysis{}, FunSigs).

check_fun_specs2(FunRefs, Specs) when is_list(FunRefs), is_map(Specs) ->
  Fun =
    fun({function, ANNO, FunRef}, Analysis) ->
      if is_map_key(FunRef, Specs) ->
        % Function reference has corresponding spec.
        Analysis;
        true ->
          % Non-existent function spec.
          Node = to_erl_af(ANNO, FunRef),
          ?pushError(?E_UNDEF__FUN_SPEC, Node, Analysis)
      end
    end,
  lists:foldl(Fun, #analysis{}, FunRefs).


%%is_valid_tag({atom, _, _}) ->
%%  true;
%%is_valid_tag(_) ->
%%  false.


%%is_valid_payload([], _) ->
%%  true;
%%is_valid_payload([{type, _, integer, _} | Elems], Types = #{}) ->
%%  is_valid_payload(Elems, Types);
%%is_valid_payload([{user_type, ANNO, Type, Vars} | Elems], Types = #{}) ->
%%
%%  % Check that the mailbox type Type is defined.
%%  case maps:is_key(Type, Types) of
%%    true ->
%%      is_valid_payload(Elems, Types);
%%    false ->
%%
%%      % Type is undefined.
%%      false
%%  end;
%%is_valid_payload([_ | _], _) ->
%%  false.


%%is_valid_payload(Types) ->
%%  lists:all(fun({type, _, Type, _}) when Type =:= integer -> true end, Types).

%%check_msg_types([{type, _, integer, _} | Types]) ->
%%  true;
%%check_msg_types([])


%%{type,172,integer, _ }

%%is_tagged_tuple() ->
%%  ok.
%%
%%is_valid_msg() ->
%%  ok.


%% Checks that the types that are mailbox specs include at least the pid. This
%% means that a mailbox type spec can be just a pid or a type union that
%% includes a pid.
%% Must check that the union consists of just tuples whose first element is an atom.
%% We also need to check that the elements of the tuple are either mailbox types
%% or primitive types.
%%check_mb_types_valid2(Types = #{}, MbSpecs = #{}) ->
%%
%%
%%  MbNames = maps:fold(fun({_, _}, {_, _, Mailbox}, Acc) -> Acc#{Mailbox => ok} end, #{}, MbSpecs),
%%  ?TRACE("MbNames = ~p", [MbNames]),
%%
%%  maps:fold(
%%    fun(Name, {_, Type, _Vars}, Error) ->
%%      ?TRACE("Name ~p, Type = ~p", [Name, Type]),
%%      case maps:is_key(Name, MbNames) of
%%        true ->
%%          ?TRACE("Checking mailbox type: ~p", [Name]),
%%
%%          % Type used in mailbox spec. Determine whether the type definition
%%          % includes the built-in pid(). There are three cases (i) the mailbox
%%          % type is a pid(), (ii) the mailbox type is type union that includes a
%%          % pid(), or (iii) the mailbox type does not include a pid(), in which
%%          % case issue a warning.
%%          case Type of
%%            {type, _, union, Elems} ->
%%
%%              ?TRACE("Elements: ~p returns: ~p", [Elems, lists:any(fun({{type, _, pid, _}}) -> true; (_) ->
%%                false end, Elems)]),
%%              % Mailbox type is a union that might or might not contain a pid().
%%
%%              case lists:any(fun({{type, _, pid, _}}) -> true; (_) -> false end, Elems) of
%%                true ->
%%                  % Not error.
%%                  ?TRACE("Contains PID"),
%%                  Error;
%%                false ->
%%                  ?TRACE("DOES NOT Contain PID"),
%%                  Error
%%              end;
%%
%%            {type, _, pid, []} ->
%%
%%              % Mailbox type is a pid().
%%              Error;
%%            Other ->
%%              % Mailbox type is a product, which is an invalid definition.
%%              % Product = incorrectly defined mailbox type.
%%
%%              % Mailbox type is a singleton which is not a pid().
%%              ?TRACE("Other = ~p", [Other]),
%%
%%              %WARN
%%              ?TRACE("DOES NOT Contain PID"),
%%              Error
%%          end;
%%        false ->
%%
%%          % Type is not used in mailbox spec.
%%          Error
%%      end
%%    end,
%%    #error{}, Types).


%% Checks that the specified term is a valid mailbox name.
%%check_is_mb_name(Name) when is_atom(Name) ->
%%  ok;
%%check_is_mb_name(_) ->
%%  {error, error_tbd}.

%%check_is_sig({Fun, Arity}) when is_atom(Fun), is_integer(Arity) ->
%%  ok;
%%check_is_sig(_) ->
%%  {error, error_tbd}.

% Duplicate use.
% Non existing signature.
% Invalid signature.
%% Checks that functions are associated with at most one mailbox modality.
%% The function signature is validated by the Erlang preprocessor.
%%check_mb_sig_redef(MbSigs = #{}) ->
%%  ok.

%% Checks that all typespecs that are mailbox types must also have the built-in
%% type pid() declared. This makes the mailbox types correct w.r.t Dialyzer.
%% Empty mailbox typespec definitions consisting of the singleton type pid()
%% are allowed.
%%check_mb_has_pid(TSpecs = #{}, MbSigs = #{}) ->
%%  maps:fold(
%%    fun(Key, {_, Type}, AccIn) ->
%%%%      case maps:is_key(Key, )
%%
%%      % Type can be a singleton or a union.
%%%%      case erl_syntax:type(Type) of
%%%%        type_union ->
%%%%          ok;
%%%%        type_application ->
%%%%          case erl_syntax:concrete(erl_syntax:type_application_name(Type)) =/= pid of
%%%%
%%%%          end;
%%%%      end,
%%
%%
%%
%%      io:format("Key: ~p, Type: ~p~n", [Key, Type]),
%%      AccIn
%%    end,
%%    #error{}, TSpecs).


%% Checks that mailbox types are defined as typespecs.
%%check_mb_defined(TSpecs = #{}, MbSigs = #{}) ->
%%  ok.


%% Checks that the functions using mailboxes are annotated with funspecs.
%% The function signature is validated by the Erlang preprocessor.
%%check_f_has_types(FSpecs = #{}, MbSigs = #{}) ->
%%  ok.

%%is_builtin_type(Name, {type, _ , Name, []}) when is_atom(Name)->
%%  true;
%%is_builtin_type(_, _) ->
%%  false.

%%is_pid_type({type, _, pid, []}) ->
%%  true;
%%is_pid_type(_) ->
%%  false.

%%has_builtin_type(Name, []) ->
%%  false;
%%has_builtin_type(Name, [Type | Types]) ->
%%  case is_builtin_type(Name, Type) of
%%    false -> has_builtin_type(Name, Type);
%%    _ -
%%  end.

%%has_pid_type(Types) ->
%%  lists:any(fun is_pid_type/1, Types).

%% TODO: After this we need to visit the tree and decorate it with annotations in preparation for the translation function on paper.
%% TODO: 1. Collect type specs.
%% TODO: 2. Collect mailbox function definition.
%% TODO: 3. Collect fun specs.

%% What Phil said: The receive annotations having a mailbox name must check that
%% that mailbox name is in scope. ie, I cannot refer to a future_mb mailbox from
%% a duncan_mb mailbox scope. This would be in another file for semantic checks
%% once we implement multiple mailboxes.


%%% ----------------------------------------------------------------------------
%%% Error handling and reporting.
%%% ----------------------------------------------------------------------------

format_sig({Fun, Arity}) ->
  io_lib:format("~s/~b", [Fun, Arity]).

%% @doc Formats the specified error to human-readable form.
format_error({?E_UNDEF__FUN_SPEC, Node}) ->
  io_lib:format(
    "function has no corresponding spec '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_MB_SIG_NOT_UNIQUE, Node}) ->
  io_lib:format(
    "fun reference '~s' associated with more than one mailbox definition",
    [erl_prettypr:format(Node)]
  );
format_error({?E_UNDEF__FUN_REF, Node}) ->
  io_lib:format(
    "mailbox interface definition contains undefined fun reference '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_UNDEF__MB_TYPE, Node}) ->
  io_lib:format(
    "undefined mailbox interface type '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__MSG_TAG, Node}) ->
  io_lib:format(
    "bad message tag '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__MSG_ELEM_TYPE, Node}) -> %TODO: See if this is used. It is but remove it and use ?E_BAD__MB_TYPE instead.
  io_lib:format(
    "bad message element type '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?W_NO__PID, Node}) ->
  io_lib:format(
    "mailbox interface type '~s' does not contain pid(), which makes it incompatible with Dialyzer",
    [erl_prettypr:format(Node)]
  );
format_error({?E_NO__MB_NEW, Node}) ->
  io_lib:format(
    "used mailbox interface '~s' is never initialized with '-new'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__MSG_TYPE, Node}) ->
  io_lib:format(
    "bad type '~s' in mailbox interface type; use a message or pid() type",
    [erl_prettypr:format(Node)]
  );
format_error({?E_BAD__MB_TYPE, Node}) ->
  io_lib:format(
    "bad type '~s' in message type; use a built-in or mailbox interface type",
    [erl_prettypr:format(Node)]
  );
format_error({?E__UNDEF_TYPE, Node}) ->
  io_lib:format(
    "undefined type '~s'",
    [erl_prettypr:format(Node)]
  ).


%%REARRANGE check_msgs_types_valid2 to use list
%%
%%REFACTOR WITH NON anonymous functions

%% REFACTOR WITH maybe

% TODO: Move to paterl syntax.
to_erl_af(ANNO, Name) when is_atom(Name) ->
  revert(set_pos(atom(Name), ANNO));
to_erl_af(ANNO, {Name, Arity})
  when is_atom(Name), is_integer(Arity) ->
  revert(set_pos(implicit_fun(atom(Name), integer(Arity)), ANNO)).







