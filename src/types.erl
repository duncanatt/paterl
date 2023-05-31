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


get_form({attribute, _, spec, {{Name, Arity}, Types}}, TInfo = #t_info{f_specs = FSpecs})
  when is_atom(Name), is_integer(Arity), is_list(Types) ->
  TInfo#t_info{f_specs = orddict:store(Name, Types, FSpecs)};

get_form({attribute, _, type, {Name, T, _}}, TInfo = #t_info{t_specs = TSpecs}) ->
  TInfo#t_info{t_specs = orddict:store(Name, T, TSpecs)};

get_form({attribute, _, Name, Sigs}, TInfo = #t_info{mb_sigs = MbSigs})
  when is_atom(Name), is_list(Sigs) ->

  

  % TODO: Refinement here to check that we only add signatures.
  TInfo#t_info{mb_sigs = orddict:store(Name, true, MbSigs)};

get_form(_, TInfo = #t_info{}) ->
  TInfo.

%%type_form({attribute, _, spec, {{Name, 0}, Types}}, TSpecs, MbSigs)
%%  when is_atom(Name), is_list(Types) ->
%%%%  Errors0 = check_fun_type_seq(Types, Errors),
%%  {TSpecs, MbSigs};


%%type_form({attribute, _, type, TSpec = {Name, T, Vars}}, TSpecs, MbSigs)
%%  when is_atom(Name), is_list(Vars) ->
%%  {[TSpec | TSpecs], MbSigs};

%% @private Wild attribute associating mailbox type to functions.
%%type_form({attribute, _, Name, Sigs}, TSpecs, MbSigs)
%%  when is_atom(Name), is_list(Sigs) ->


%% Checks that types in the table are consistent and existing.
check() -> ok.

-spec format_errors(any()) -> ok.
format_errors(_) ->
  ok.