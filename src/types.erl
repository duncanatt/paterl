%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. May 2023 15:01
%%%-------------------------------------------------------------------
-module(types).
-author("duncan").

-compile(export_all).
%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").

%%% Public API.
-export([]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(E_MB_TYPE_NOT_DECL, e_mb_type_not_decl).
-define(E_MB_TYPE_NO_PID, e_mb_type_no_pid).

%% Error creation macros.
-define(
pushError(Class, Node, Errors),
  [{element(2, Node), ?MODULE, {Class, Node}} | Errors]
).
-define(
pushError(Class, Reason, Node, Errors),
  [{element(2, Node), ?MODULE, {Class, Reason, Node}} | Errors]
).


%% Program type information.
-record(t_info, {
  t_specs = [] :: [spec()],
  f_specs = [] :: [spec()],
  mb_sigs = [] :: [signature()]
}).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type signature() :: {atom(), arity()}.
%% Function signature.

-type spec() :: {atom(), erl_syntax:erl_parse()}.
%% Type and function spec.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------


%%-spec get(erl_syntax:syntaxTree()) -> ok.
get(Forms) ->
  get_forms(Forms, #t_info{}).
%%  ok.








get_forms([], Table) ->
  Table;
get_forms([Form | Forms], Table) ->
  ?DEBUG("Processing form: ~p", [Form]),
  Table0 = get_form(Form, Table),
  get_forms(Forms, Table0).


get_form(Type = {attribute, _, spec, {{Name, Arity}, FTypes}}, TInfo = #t_info{f_specs = FSpecs})
  when is_atom(Name), is_integer(Arity), is_list(FTypes) ->


  [{type, 120, 'fun',
    [{type, 120, product,
      [{user_type, 120, future, []},
        {type, 120, atom, []}]},
      {type, 120, integer, []}]}
  ],

  [{type, 120, 'fun',
    [{type, 120, product,
      [{user_type, 120, future, []},
        {type, 120, atom, []}]},
      {type, 120, integer, []}]},
    {type, 120, 'fun',
      [{type, 120, product,
        [{user_type, 120, future, []},
          {type, 120, atom, []}]},
        {type, 120, float, []}]}
  ],

%%  {type,_,'fun',[{type,_,product,Types},T_0]}


  TInfo#t_info{f_specs = orddict:store(Name, {erl_syntax:revert(erl_syntax:attribute_name(Type)), FTypes}, FSpecs)};

get_form({attribute, _, type, {Name, T, _}}, TInfo = #t_info{t_specs = TSpecs}) ->
  TInfo#t_info{t_specs = orddict:store(Name, T, TSpecs)};

%% @private Wild attribute associating mailbox type to functions.
get_form(Type = {attribute, _, Name, Sigs}, TInfo = #t_info{mb_sigs = MbSigs})
  when is_atom(Name), Name =/= export, is_list(Sigs) ->

  % Predicate determines whether the specified tuple is a signature.
  IsSig =
    fun({F, A}, Flag) when is_atom(F), is_integer(A) -> Flag and true;
      (_, _) -> false
    end,


  case lists:foldl(IsSig, true, Sigs) of
    true ->
      TInfo#t_info{mb_sigs = orddict:store(Name, {erl_syntax:revert(erl_syntax:attribute_name(Type)), Sigs}, MbSigs)};
    false ->
      TInfo
  end;

get_form(_, TInfo = #t_info{}) ->
  TInfo.


%%type_form({attribute, _, type, TSpec = {Name, T, Vars}}, TSpecs, MbSigs)
%%  when is_atom(Name), is_list(Vars) ->
%%  {[TSpec | TSpecs], MbSigs};


%%type_form({attribute, _, Name, Sigs}, TSpecs, MbSigs)
%%  when is_atom(Name), is_list(Sigs) ->


%% Checks that types in the table are consistent and existing.
% ok, {errors, []}
check(TInfo = #t_info{}) ->


  case check_mb_sigs(TInfo) of
    [] ->
%%      check_mb_types(TInfo);
      check_f_specs(TInfo);
    Errors ->
      Errors
  end.


%% @private Checks that the mailbox interface name that associates the mailbox
%% to the functions adhering to it is defined as a type spec. Should be the last.
check_mb_sigs(#t_info{t_specs = TSpecs, mb_sigs = MbSigs}) ->

  %% Checks that the mailbox interface name is declared as a type spec.
  HasTypeSpec =
    fun(Name, {A, _}, Errors) ->
      case orddict:is_key(Name, TSpecs) of
        true ->
          Errors; % Mailbox type declared.
        false ->
          ?pushError(?E_MB_TYPE_NOT_DECL, A, Errors) % Mailbox type not declared.
      end
    end,
  orddict:fold(HasTypeSpec, [], MbSigs).

%% @private Checks that user-defined type specs referenced in function specs
%% exist.
%% Should be the first lightweight check. Assumes nothing.
check_f_specs(#t_info{t_specs = TSpecs, f_specs = FSpecs}) ->

  %% 1. For each FSpec in FSpecs
  %%    a. Get the product list of parameters and for each element in the list
  %%       i. Check that it is a key in TSpecs.
  %%    b. Get the return type and check that it is a key in TSpecs.


  % FOR now we support exactly on function type spec.
  HasUserDefinedTypes =
    fun(Key, {N, [{type, _, 'fun', [{type, _, product, Types}, T_0]}]}, Errors) ->
      ?TRACE("Name is ~p, Types are ~p", [Key, Types]),

      lists:foldl(
        fun(T = {user_type, _, UTypeName, []}, E) ->
          case orddict:is_key(UTypeName, TSpecs) of
            true ->
              E;
            false ->
              ?pushError(non_existent_user_type, T, E)
          end;
          (_, E) ->
            E
        end,
        Errors, [T_0 | Types]);

      (_, {N, [_, _ | _]}, Errors) ->
        ?pushError(mult_fun_typespec_not_supported, N, Errors)
    end,


  orddict:fold(HasUserDefinedTypes, [], FSpecs).


%% @private Checks that the mailbox interface name that associates the mailbox
%% to the functions adhering to it includes the built-in type 'pid' as its type
%% or in its type union.
check_mb_types(#t_info{t_specs = TSpecs, mb_sigs = MbSigs}) ->


  ?DEBUG("TSpecs = ~p", [TSpecs]),

  HasPidType =
    fun(Name, {Atom, _}, Errors) ->
      ?TRACE("TYPE Checking name ~p", [Name]),

      %% We assume that it exists based on the previous checking.
      ?assert(orddict:is_key(Name, TSpecs)),
      case orddict:fetch(Name, TSpecs) of
        {type, _, pid, []} ->
          Errors;
        {type, _, union, Types} ->
          case has_pid_type(Types) of
            true ->
              Errors;
            false ->
%%              [Name | Errors]
              ?pushError(?E_MB_TYPE_NO_PID, Atom, Errors)
          end;
        {user_type, _, _, []} ->
          ?pushError(?E_MB_TYPE_NO_PID, Atom, Errors)
%%          [Name | Errors]
      end
    end,


  orddict:fold(HasPidType, [], MbSigs).


check_f_sigs(FSigs, Forms) when is_list(FSigs), is_list(Forms) ->
  orddict:fold(
    fun(Key, Val, Errors) ->
      Errors
    end, [], FSigs).


%%is_pid_type({type, _, pid, []}) ->
%%  true;
%%is_pid_type(_) ->
%%  false.

has_pid_type([]) ->
  false;
has_pid_type([{type, _, pid, []} | _]) ->
  true;
has_pid_type([_ | Types]) ->
  has_pid_type(Types).


%% 1. Check that the function signature references (values) in mb_sigs refer to
%% functions defined in forms. This ensures that these references describe
%% existing functions. Why? To check that the function specs are valid.

%% 2. Check that the function signature references (values) in mb_sigs exist
%% in the f_specs table. This ensures that these references are type-annotated.
%% Error: Function reference F/A is not annotated.
%% Why? To detect missing function spec annotations for the functions that
%% are used by mailbox interfaces.

%% 2. Check that the function names (keys) in mb_sigs are defined as types in
%% t_specs.







%%
%%
%%
%% that use
%% a user-defined type is defined as a user type in t_specs.




%% 2. Check that the function signatures in f_specs that use a user_type type is
%% found in the t_specs.

%% 3. Check that the keys in mb_sigs have a pid() type in the type union defined
%% in t_specs.





-spec format_error(any()) -> ok.
format_error({?E_MB_TYPE_NOT_DECL, Node}) ->
  io_lib:format("mailbox type '~s' not declared", [erl_prettypr:format(Node)]);
format_error({?E_MB_TYPE_NO_PID, Node}) ->
  io_lib:format(
    "illegal mailbox type '~s' must contain pid() built-in type",
    [erl_prettypr:format(Node)]
  );
format_error(E) ->
  io_lib:format("unknown error ~p", [E]).