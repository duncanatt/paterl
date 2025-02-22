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
-include("log.hrl").
-include("paterl_lib.hrl").

%%% Public API.
-export([module/2]).


%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% Launcher function name.
-define(MAIN__FUN_NAME, main).

%% Launcher function name.
-define(MAIN__FUN_ARITY, 0).

%% Creates the bootstrap function name.
-define(launchFunName(Name), list_to_atom(atom_to_list(Name) ++ "'")).


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-doc """
Appends the bootstrap main function to the list of `Forms`.

### Returns
- list of `Forms` with the **annotated** bootstrap main function
""".
module(Forms, TypeInfo = #type_info{spec_defs = Specs, mb_funs = MbDefs}) ->

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
  TypeInfo0 = TypeInfo#type_info{spec_defs = Specs0},

  % Create bootstrap function using the interface of the main user-defined
  % function.
  {_, _, Interface} = maps:get({?MAIN__FUN_NAME, ?MAIN__FUN_ARITY}, MbDefs),
  ?TRACE("Found '~s/~b' function interface '~s' to boostrap.", [
    ?MAIN__FUN_NAME, ?MAIN__FUN_ARITY, Interface
  ]),
  BsFun = bs_fun_def(BsFunName),
  {Forms ++ [BsFun], TypeInfo0}.


%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

-doc """
Creates the bootstrap main function that is added to complete the generated Pat
code in later passes.
""".
bs_fun_def(BsFunName) when is_atom(BsFunName) ->
  % Create bootstrap main function return type.
  RetType = erl_syntax:revert(
    erl_syntax:type_application(erl_syntax:atom(any), [])
  ),

  % Create bootstrap main function.
  Call = erl_syntax:application(erl_syntax:atom(main), []),
  Clause = erl_syntax:clause([], [Call]),

  % Annotate bootstrap main function with return type.
  % TODO: Might change it later and place the bootstrapping module later in the
  % TODO: pipeline to reduce handling this module as a special case.
  paterl_syntax:set_anno(
    erl_syntax:function(erl_syntax:atom(BsFunName), [Clause]),
    paterl_anno:set_type(RetType, erl_anno:new(0))
  ).