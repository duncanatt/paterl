%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jun 2023 11:30
%%%-------------------------------------------------------------------
-module(types_typespecs).
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

-define(E_MB_TYPE_UNDEF, e_mb_type_undef).
-define(W_MB_TYPE_PID, w_mb_type_pid).
-define(W_MB_TYPE_EMPTY, w_mb_type_empty).
%%-define(E_MB_TYPE_INVALID, e_mb_type_invalid).
-define(E_MB_SIG_TYPE_UNDEF, e_mb_sig_type_undef).


%%%% Error creation macros.
-define(
pushError(Class, Node, Errors),
%%  [{element(2, Node), ?MODULE, {Class, Node}} | Errors]
  [{erl_syntax:get_pos(Node), ?MODULE, {Class, Node}} | Errors]
).
-define(
pushError(Class, Reason, Node, Errors),
%%  [{element(2, Node), ?MODULE, {Class, Reason, Node}} | Errors]
  [{erl_syntax:get_pos(Node), ?MODULE, {Class, Reason, Node}} | Errors]
).

%% Program type information.
-record(t_info, {
  t_specs = [] :: [t_spec()],
  f_specs = [] :: [f_spec()],
  mb_sigs = [] :: [mb_sig()]
}).


%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type signature() :: {atom(), arity()}.
%% Function signature as Fun/Arity.

-type name() :: atom().
%% Type or function spec name.

-type mailbox() :: atom().
%% Mailbox interface name.

-type t_spec() :: {name(), {erl_syntax:erl_parse(), erl_syntax:erl_parse()}}.
%% Type specs map.

-type f_spec() :: {{name(), arity()}, {erl_syntax:erl_parse(), erl_syntax:erl_parse()}}.
%% Function specs map.

-type mb_sig() :: {mailbox(), {erl_syntax:erl_parse(), [signature()]}}.
%% Mailbox interface names map that tracks the Erlang functions implementing the
%% mailbox type.

-type t_info() :: #t_info{}.
%% Program type information.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-spec table(erl_syntax:forms()) -> t_info().
table(Forms) ->
  TInfo = get_types(Forms, #t_info{}),

  ?TRACE("TInfo~n~s", [lists:duplicate(80, $-)]),
  ?TRACE("t_specs: ~p", [TInfo#t_info.t_specs]),
  ?TRACE("f_specs: ~p", [TInfo#t_info.f_specs]),
  ?TRACE("mb_sigs: ~p~n~n", [TInfo#t_info.mb_sigs]),


  case check_mb_types(TInfo#t_info.mb_sigs, TInfo#t_info.t_specs) of
    {[], Warnings0} ->
      case check_mb_has_pid(TInfo#t_info.mb_sigs, TInfo#t_info.t_specs) of
        {[], Warnings1} ->
          case check_mb_sig_types(TInfo#t_info.mb_sigs, TInfo#t_info.f_specs) of
            {[], Warnings2} ->
              {ok, TInfo, Warnings0 ++ Warnings1 ++ Warnings2};
            {Errors2, Warnings2} ->
              ?ERROR("----- Errros were found!"),
              {errors, Errors2, Warnings0 ++ Warnings1 ++ Warnings2}
          end;
        {Errors1, Warnings1} ->
          {errors, Errors1, Warnings0 ++ Warnings1}
      end;
    {Errors0, Warnings0} ->
      {errors, Errors0, Warnings0}
  end.




get_types([], TInfo = #t_info{}) ->
  TInfo;
get_types([Form | Forms], TInfo = #t_info{}) ->
  TInfo0 = get_type(Form, TInfo),
  get_types(Forms, TInfo0).


get_type(Form = {attribute, _, spec, {{Name, Arity}, FTypes}}, TInfo = #t_info{f_specs = FSpecs})
  when
  is_atom(Name), is_integer(Arity), is_list(FTypes) ->

  Node = erl_syntax:set_pos(erl_syntax:atom(Name), erl_syntax:get_pos(Form)),
  TInfo#t_info{f_specs = orddict:store({Name, Arity}, {Node, FTypes}, FSpecs)};

get_type(Form = {attribute, _, type, {Name, T, []}}, TInfo = #t_info{t_specs = TSpecs})
  when
  is_atom(Name) ->

  Node = erl_syntax:set_pos(erl_syntax:atom(Name), erl_syntax:get_pos(Form)),
  TInfo#t_info{t_specs = orddict:store(Name, {Node, T}, TSpecs)};

get_type(Form = {attribute, _, Name, Sigs}, TInfo = #t_info{mb_sigs = MbSigs})
  when
  is_atom(Name), Name =/= export, is_list(Sigs) ->
  case is_signatures(Sigs) of
    true ->
      Node = erl_syntax:attribute_name(Form),
      TInfo#t_info{mb_sigs = orddict:store(Name, {Node, Sigs}, MbSigs)};
    false ->
      TInfo
  end;

get_type(_, TInfo = #t_info{}) ->
  TInfo.


%%% ----------------------------------------------------------------------------
%%% Type consistency checking functions.
%%% ----------------------------------------------------------------------------


%% 1. Aim: To check that the mailbox interface names (which associate Erlang
%% functions to the mailbox interfaces) are declared as type specs.
%%
%% How: For each key in mb_sigs check that it is a key in t_specs.
%% Error: Mailbox interface '~s' is not defined as a type spec.

-spec check_mb_types([mb_sig()], [t_spec()]) -> {Warnings, Errors}
  when
  Warnings :: errors:errors(),
  Errors :: errors:errors().
check_mb_types(MbSigs, TSpecs) ->
  orddict:fold(
    fun(MbName, {Node, _}, {Errors, Warnings}) ->
      case orddict:is_key(MbName, TSpecs) of
        true ->
          {Errors, Warnings};
        false ->
          {?pushError(?E_MB_TYPE_UNDEF, Node, Errors), Warnings}
      end
    end,
    {[], []}, MbSigs).


%% 2. Aim: To check that the mailbox interface names have the built-in 'pid'
%% type associated with them, thereby enabling other tools like Dialyzer to
%% type check them successfully.
%%
%% Assumes: That an entry for the mailbox interface name of mb_specs exists in
%% t_specs.
%% How: For each key in mb_specs, find the corresponding type in t_specs, and
%% check that its type spec definition is (i) either a 'pid' or (ii) is a type
%% union that itself contains a 'pid'. For now, the use of 'pid' is direct, i.e.
%% we do not recurse in case we find a user-defined type, and check whether that
%% type itself contains a 'pid', and so on.
%% Warn: Type spec '~s' requires also type 'pid()'
-spec check_mb_has_pid([mb_sig()], [t_spec()]) -> {Warnings, Errors}
  when
  Warnings :: errors:errors(),
  Errors :: errors:errors().
check_mb_has_pid(MbSigs, TSpecs) ->
  orddict:fold(
    fun(MbName, {Node, _}, {Errors, Warnings}) ->

      % Assume that mailbox interface name is defined as a type spec.
      ?assert(orddict:is_key(MbName, TSpecs)),
      case orddict:fetch(MbName, TSpecs) of
        {_, {type, _, pid, []}} ->

          % Mailbox type defined as the built-in type 'pid'. While this is a
          % valid Erlang type spec, it corresponds to an empty mailbox interface
          % in Pat.
          {Errors, ?pushError(?W_MB_TYPE_EMPTY, Node, Warnings)};

        {_, {type, _, union, Types}} ->

          % Check that type union for mailbox type spec contains built-in type
          % 'pid'.
          case has_builtin(pid, Types) of
            true ->

              % Mailbox type union contains built-in type 'pid'.
              {Errors, Warnings};
            false ->

              % Mailbox type union does not contain built-in type 'pid'. While
              % this is a valid Pat mailbox interface definition, it does not
              % type check under Dialyzer.
              {Errors, ?pushError(?W_MB_TYPE_PID, Node, Errors)}
          end;
        {_, _} ->

          % Mailbox type not defined as the built-in type 'pid'. While this is a
          % valid Pat mailbox interface definition, it does not type check under
          % Dialyzer.
          {Errors, ?pushError(?W_MB_TYPE_PID, Node, Warnings)}
      end
    end,
    {[], []}, MbSigs).


%% 3. Aim: To check that the functions that are instantiated to processes the
%% mailbox interface have the necessary type information.
%%
%% How: For each key in mb_specs, for each of its F/A in its values, check that
%% there is a key in the f_specs table. As a key, I might have to use the
%% {name, arity}, rather than the name alone.
%% Error: Function F/A used in mailbox interface '~s' is not annotated.


%% @private Checks that the Erlang functions implementing the mailbox type are
%% annotated with type information.
-spec check_mb_sig_types([mb_sig()], [f_spec()]) -> {Warnings, Errors}
  when
  Warnings :: errors:errors(),
  Errors :: errors:errors().
check_mb_sig_types(MbSigs, FSpecs) ->
  orddict:fold(
    fun(MbName, {Node, Sigs}, {Errors, Warnings}) ->

      ?TRACE("Processing mbname: ~p", [MbName]),
      lists:foldl(
        fun(Sig, EW = {Errors0, _}) ->
          ?TRACE("Looking up sig ~p", [Sig]),
          ?TRACE("FSpecs ~w", [FSpecs]),
          case orddict:is_key(Sig, FSpecs) of
            true ->
              ?TRACE("Function ~p has been found", [Sig]),
              EW;
            false ->

              FA = erl_syntax:set_pos(
                erl_syntax:atom(
                  lists:flatten(io_lib:format("~s/~w", tuple_to_list(Sig)))
                ),
                erl_syntax:get_pos(Node)),

              ?ERROR("Function ~p was not found.", [Sig]), % Need a function name for the error.
              {?pushError(?E_MB_SIG_TYPE_UNDEF, FA, Errors0), Warnings}
          end
        end,
        {Errors, Warnings}, Sigs)
    end,
    {[], []}, MbSigs).


%%% ----------------------------------------------------------------------------
%%% Helper functions.
%%% ----------------------------------------------------------------------------

%% @private Determines whether the elements in specified list are function
%% signatures.
is_signatures([]) ->
  false;
is_signatures([{F, A}]) when is_atom(F), is_integer(A) ->
  true;
is_signatures([{F, A} | Sigs]) when is_atom(F), is_integer(A) ->
  is_signatures(Sigs);
is_signatures([_ | _]) ->
  false.

%% @private Determines whether the specified list of types contains a type with
%% name. TODO: May be improved by extending it wil the different type patterns
%% in sec 8.7 of the EAF spec to search recursively for the type, and make it
%% more explicit in that if no pattern matches, the function fails.
has_builtin(_, []) ->
  false;
has_builtin(N, [{type, _, N, _} | _]) ->
  true;
has_builtin(N, [_ | Types]) ->
  has_builtin(N, Types).


%%% ----------------------------------------------------------------------------
%%% Error handling and reporting.
%%% ----------------------------------------------------------------------------

%% @doc Formats the specified error to human-readable form.
format_error({?E_MB_TYPE_UNDEF, Node}) ->
  io_lib:format(
    "mailbox interface '~s' has no corresponding typespec defined",
    [erl_prettypr:format(Node)]
  );
format_error({?W_MB_TYPE_PID, Node}) ->
  io_lib:format(
    "mailbox typespec definition '~s' missing built-in type pid()",
    [erl_prettypr:format(Node)]
  );

format_error({?W_MB_TYPE_EMPTY, Node}) ->
  io_lib:format("mailbox interface '~s' is empty", [erl_prettypr:format(Node)]);

%%format_error({?E_MB_TYPE_INVALID, Node}) ->
%%  io_lib:format(
%%    "mailbox interface has invalid type '~s'", [erl_prettypr:format(Node)]
%%  );

format_error({?E_MB_SIG_TYPE_UNDEF, Node}) ->
  io_lib:format(
    "no type information found for function ~s used in mailbox interface",
    [erl_prettypr:format(Node)]
  ).


