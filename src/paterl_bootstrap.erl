%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2024 16:48
%%%-------------------------------------------------------------------
-module(paterl_bootstrap).
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include_lib("syntax_tools/include/merl.hrl").
-include("log.hrl").
-include("errors.hrl").
-include("paterl.hrl").

%% API
-export([]).
-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Launcher function name.
-define(MAIN__FUN_NAME, main).

%% Launcher function name.
-define(MAIN__FUN_ARITY, 0).

-define(launchFunName(Name), list_to_atom(atom_to_list(Name) ++ "'")).


forms(Forms, TInfo = #t_info{specs = Specs, mb_defs = MbDefs}) ->

  % Bootstrap function name. The auxiliary bootstrapping function closes the
  % program and serves as the main entry point that launches the main function
  % defined by the user. The assumed user-defined main function is controlled by
  % the ?MAIN__FUN_NAME and ?MAIN__FUN_ARITY macros.
  BsFunName = list_to_atom(atom_to_list(?MAIN__FUN_NAME) ++ "'"),

  % Add typespec definition information to the type info record.
  FunType = erl_syntax:revert(
    erl_syntax:function_type(
      [], erl_syntax:type_application(erl_syntax:atom(any), [])
    )),
  Specs0 = Specs#{
    {BsFunName, erl_anno:new(0)} => {spec, erl_anno:new(0), [FunType]}
  },
  TInfo0 = TInfo#t_info{specs = Specs0},

  % Create bootstrap function using the interface of the main user-defined
  % function.
  {_, _, Interface} = maps:get({?MAIN__FUN_NAME, ?MAIN__FUN_ARITY}, MbDefs),
  ?TRACE("Found '~s/~b' function interface '~s' to boostrap.", [
    ?MAIN__FUN_NAME, ?MAIN__FUN_ARITY, Interface
  ]),
  BsFun = bs_fun_def(BsFunName, Interface),
  {Forms ++ [BsFun], TInfo0}.

%%add_launch_fun_types(Specs) ->
%%
%%  % Main function key.
%%  Key = {?MAIN__FUN_NAME, ?MAIN__FUN_ARITY},
%%
%%%%  {_Modality, _, Interface} = maps:get(Key, ),
%%
%%  RetType = erl_syntax:type_application(erl_syntax:atom(no_return), []),
%%  FunType = erl_syntax:function_type([], RetType),
%%
%%
%%
%%
%%  Specs0 = Specs#{{?launchFunName(?MAIN__FUN_NAME), ?MAIN__FUN_ARITY} => {spec, 0, erl_syntax:revert(FunType)}},
%%  TInfo#t_info{specs = Specs0}.

%% @private Creates the bootstrap main function that is added to complete the
%% generated Pat code in later passes.
bs_fun_def(BsFunName, Interface) ->
  Type = erl_syntax:revert(
    erl_syntax:type_application(erl_syntax:atom(any), [])
  ),

  Anno = erl_syntax:tuple([erl_syntax:atom(new), erl_syntax:atom(Interface)]),
  Call = erl_syntax:application(erl_syntax:atom(main), []),
  Clause = erl_syntax:clause([], [Anno, Call]),

  erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:function(erl_syntax:atom(BsFunName), [Clause]),
      paterl_anno:set_type(Type, erl_anno:new(0))
    )).


%%launch_fun_name(Name) when is_atom(Name), Name =:= ?MAIN__FUN_NAME ->
%%  list_to_atom(atom_to_list(Name) ++ "'").
