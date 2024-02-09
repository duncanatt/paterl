%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% The Pat Erlang type management.
%%% @end
%%% Created : 29. Jan 2024 15:23
%%%-------------------------------------------------------------------
-module(paterl_types).
-author("duncan").

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
-include("log.hrl").
-include("errors.hrl").

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

-compile(export_all).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

-define(M_NEW, new).
-define(M_USE, use).

-define(T_TYPE, type).
-define(T_SPEC, spec).
-define(T_MBOX, mbox).

%%% Error types.

%% Mailbox specification syntactically incorrect.
-define(E_MB_SPEC_BAD, e_mb_spec_bad).

%% Invalid function Fun/Arity signature.
-define(E_MB_SIG_BAD, e_mb_sig_bad).

%% Function signature associated with more than one mailbox type.
-define(E_MB_SIG_NOT_UNIQUE, e_mb_sig_not_unique). %%

%% Undefined function signature.
-define(E_MB_SIG_UNDEF, e_mb_sig_undef).

%% Undefined mailbox type.
-define(E_MB_TYPE_UNDEF, e_mb_type_undef).

%% Invalid message tag.
-define(E_MB_MSG_TAG_BAD, e_mb_msg_tag_bad).

%% Invalid message element type. Supported types are integers and strings.
-define(E_MB_MSG_ELEM_TYPE_BAD, e_mb_msg_elem_type_bad).

-define(W_MB_NO_PID, w_mb_no_pid).


%%-define(E_MB_TYPE_UNDEF, e_mb_type_undef).
-define(W_MB_TYPE_PID, w_mb_type_pid).
-define(W_MB_TYPE_EMPTY, w_mb_type_empty).
%%-define(E_MB_TYPE_INVALID, e_mb_type_invalid).
%%-define(E_MB_SIG_TYPE_UNDEF, e_mb_sig_type_undef).


%% Error macros.

%% Program type information.
%%-record(p_types, {
%%  t_specs = [] :: [t_spec()], % Global type def names to AST.
%%  f_specs = [] :: [f_spec()], % Function signatures to AST.
%%  mb_sigs = [] :: [mb_sig()]  % Mailbox names to mailbox modality and function signatures.
%%}).



-record(t_info, {
  types = #{} :: types(), % Global type def names to AST.
  specs = #{} :: specs(), % Function signatures to AST.
  mb_defs = #{} :: mb_defs(),  % Mailbox names to mailbox modality and function signatures.
  mb_names = [] :: [mailbox()]
}).

%%-record(p_data, {
%%  f_sigs = [] :: [signature()] %% List of function signatures.
%%}).

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type signature() :: {function(), arity()}.
%% Function signature Name/Arity type.

-type name() :: atom().
%% Type spec or function spec name.

-type mailbox() :: atom().
%% Mailbox type name.

-type modality() :: ?M_NEW | ?M_USE.

-type ast() :: erl_syntax:erl_parse().

%%-type t_spec() :: {spec(), {erl_syntax:erl_parse(), erl_syntax:erl_parse()}}.

%%-type signatures() :: [signature()].

-type types() :: #{name() => {ast(), ast()}}.

%%-type f_spec() :: {signature(), {erl_syntax:erl_parse(), erl_syntax:erl_parse()}}.
-type specs() :: #{signature() => {ast(), ast()}}.

%%-type mb_sig() :: {mailbox(), {new | use, [signature()]}}.
%%-type mb_sigs() :: #{mailbox() => {new | use, [signature()]}}.

%%-type mb_specs() :: #{signature() => [{modality(), mailbox()}]}.
-type mb_defs() :: [{{modality(), mailbox()}, [signature()]}].


%% Mailbox interface names map that tracks the Erlang functions implementing the
%% mailbox type.

-type t_info() :: #t_info{}.
%% Program type information.


%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

-spec table(erl_syntax:forms()) -> t_info().
table(Forms) ->
  case check_mb_specs_syntactic(Forms) of % TODO: Move it to the syntax checking file.
    {ok, _} ->

      % Extract type information from AST to table.
      case get_t_info(Forms) of
        {ok, TInfo = #t_info{}, []} ->

          #t_info{types = Types, specs = Specs, mb_defs = MbDefs, mb_names = MbNames} = TInfo,

          io:format("~n~s SIGS & TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),
          io:format("Types: ~p~n", [Types]),
          io:format("Specs: ~p~n", [Specs]),
          io:format("MbDefs: ~p~n", [MbDefs]),
          io:format("MbNames: ~p~n", [MbNames]),
          io:format("~s SIGS & TINFO ~s~n", [lists:duplicate(40, $-), lists:duplicate(40, $-)]),

          case check_mb_types_defined(MbNames, Types) of
            {ok, []} ->
              case check_mb_types_valid(Types) of
                {ok, Warnings} ->

                  %% Final. Check that all functions are type speced.

                  {ok, TInfo, Warnings};
                Error = #error{} ->
                  % One or more invalid messages types in mailbox definitions.
                  Error
              end;
            Error = #error{} ->
              % One or more undefined mailbox type names.
              Error
          end;
        Error = #error{} ->
          % One or more function signatures associated with the same mailbox
          % definition or undeclared function signatures in mailbox definitions.
          Error
      end;
    Error = #error{} ->
      % Syntax errors in mailbox definitions.
      Error
  end.

% {Sigs, MbSigs, Types, Specs}
read_attribs(Forms) ->
  lists:foldl(
    fun({function, ANNO, Fun, Arity, _}, {Sigs, MbSigs, Types, Specs}) ->
      ?TRACE("~b: ~s/~b", [ANNO, Fun, Arity]),
%%      {new_sig(ANNO, {Fun, Arity}, Sigs), Sigs};
      {new_sig(ANNO, {Fun, Arity}, Sigs), MbSigs, Types, Specs};
      ({attribute, ANNO, Modality, {Mailbox, Sigs0}}, {Sigs, MbSigs, Types, Specs})
        when Modality =:= ?M_NEW; Modality =:= ?M_USE ->
        ?TRACE("~b: -~s ~s with ~p", [ANNO, Modality, Mailbox, Sigs0]),
%%        {Sigs, new_mb_sigs(ANNO, Sigs0, Modality, Mailbox, TInfo)};
        {Sigs, new_mb_def(ANNO, Sigs0, Modality, Mailbox, MbSigs), Types, Specs};

      %{attribute, ANNO, type, {Name, Rep(T), [Rep(V_1), ..., Rep(V_k)]}}
      ({attribute, ANNO, type, {Name, Type, Vars}}, {Sigs, MbSigs, Types, Specs}) ->
        ?TRACE("~b: -type ~s", [ANNO, Name]),
%%        {Sigs, put_type(Name, Type, Defs, TInfo)};
        {Sigs, MbSigs, new_type(ANNO, Name, Type, Vars, Types), Specs};

      %{attribute, ANNO, spec, {Sig = {Name,Arity}, [Rep(Ft_1), ..., Rep(Ft_k)]}}.
      ({attribute, ANNO, spec, {Sig = {Name, Arity}, Types0}}, {Sigs, MbSigs, Types, Specs}) ->
        ?TRACE("~b: -spec ~s/~b", [ANNO, Name, Arity]),
%%        {Sigs, put_spec(Sig, Body, TInfo)};
        {Sigs, MbSigs, Types, new_spec(ANNO, Sig, Types0, Specs)};
      (_Form, Data) ->
        ?TRACE("~b: ~s ~p", [element(2, _Form), element(1, _Form),
          if size(_Form) > 2 -> element(3, _Form); true -> undefined end]),
        Data
    end, {[], [], [], []}, Forms).


get_t_info(Forms) ->

  % Read raw attributes from AST. The consistency of type and function
  % specifications is performed by the Erlang preprocessor.
  {Sigs, MbDefs, Types, Specs} = read_attribs(Forms),

  ?TRACE("Sigs: ~p", [Sigs]),
  ?TRACE("MbDefs: ~p", [MbDefs]),
  ?TRACE("Types: ~p", [Types]),
  ?TRACE("Specs: ~p", [Specs]),

  % Get mailbox definition map for convenient use for type consistency checking.
  % Check that each function signature is associated to at most one mailbox
  % definition and modality.
  return(
    case make_mb_defs(MbDefs) of
      {ok, MbDefs0 = #{}, []} ->

        % Check that function signatures used in mailbox definitions are defined.
        case check_mb_sigs_defined(MbDefs0, Sigs) of
          {ok, []} ->

            % Get unique mailbox names.
            MbNames = make_mb_names(MbDefs),

            % Type info record.
            #t_info{
              types = make_types(Types, MbNames),
              specs = make_specs(Specs, MbDefs0),
              mb_names = MbNames,
              mb_defs = MbDefs0
            };
          Error = #error{} ->
            % One or more function signatures in mailbox definitions are not
            % declared as types.
            Error
        end;
      Error = #error{} ->
        % One or more function signatures are associated with the same mailbox.
        Error
    end
  ).


new_sig(ANNO, Sig = {_, _}, Sigs) when is_list(Sigs) ->
  [Sig | Sigs].

new_mb_def(ANNO, Sigs, Modality, Mailbox, MbSpecs)
  when is_list(Sigs), is_list(MbSpecs) ->
  [{{Modality, Mailbox}, {ANNO, Sigs}} | MbSpecs].

%%put_type(ANNO, Name, Type, Vars, Types = #{}) when is_list(Vars) ->
%%  Types#{Name => {ANNO, Type, Vars}}.

new_type(ANNO, Name, Type, Vars, Types) when is_list(Vars), is_list(Types) ->
  [{Name, {ANNO, Type, Vars}} | Types].

%%put_spec(ANNO, Sig = {_, _}, Types, Specs = #{}) when is_list(Types) ->
%%  Specs#{Sig => {ANNO, Types}}.

new_spec(ANNO, Sig = {_, _}, Types, Specs) when is_list(Types), is_list(Specs) ->
  [{Sig, {ANNO, Types}} | Specs].


%% @private Returns the map of mailbox names defined.
%% @returns Mailbox names.
make_mb_names(MbDefs) when is_list(MbDefs) ->
  lists:foldl(
    fun({{_, Name}, {ANNO, _}}, Names) ->
      Names#{Name => {ANNO}}
    end,
    #{}, MbDefs).

make_types(Types, MbNames = #{}) when is_list(Types) ->
%%  Mailboxes = lists:map(fun({_, _, Name}) -> Name end, maps:values(MbDefs)),
  lists:foldl(
    fun({Name, {ANNO, Type, Vars}}, Map) ->
      case maps:is_key(Name, MbNames) of
%%      case maps:take(Name, MbNames) of %TODO: Optimisation if we use take!
        true ->

          % Type defines a mailbox.
          Map#{Name => {?T_MBOX, ANNO, Type, Vars}};
        false ->

          % Type does not define a mailbox.
          Map#{Name => {?T_TYPE, ANNO, Type, Vars}}
      end
    end,
    #{}, Types).


make_specs(Specs, MbDefs = #{}) when is_list(Specs) ->
%%  Sigs = maps:keys(MbDefs),
  lists:foldl(
    fun({Sig, {ANNO, Types}}, Map) ->
%%      case lists:member(Sig, Sigs) of
      case maps:is_key(Sig, MbDefs) of
        true ->

          % Function uses a mailbox.
          Map#{Sig => {?T_MBOX, ANNO, Types}};
        false ->

          % Function does not use a mailbox.
          Map#{Sig => {?T_SPEC, ANNO, Types}}
      end
    end,
    #{}, Specs).


%%% ----------------------------------------------------------------------------
%%% Syntactic checking functions.
%%% ----------------------------------------------------------------------------

% 1. Check that the new and use attributes have the correct format.
% TODO: Maybe move this to the other module concerned with syntactic checking
% TODO: and leave this module only for semantics/consistency checks.
check_mb_specs_syntactic(Forms) when is_list(Forms) ->
  return(
    lists:foldl(
      fun(Node = {attribute, _, Modality, {Mailbox, Sigs}}, Error)
        when
        Modality =:= ?M_NEW, is_atom(Mailbox), is_list(Sigs);
        Modality =:= ?M_USE, is_atom(Mailbox), is_list(Sigs) ->
        lists:foldl(
          fun({Name, Arity}, Error) when is_atom(Name), Arity >= 0, Arity =< 255 ->
            Error;
            (_, Error = #error{}) ->

              % Invalid function signature. Other signature forms, such as
              % atom/atom are checked by the Erlang preprocessor.
              ?pushError(?E_MB_SIG_BAD, Node, Error)
          end, Error, Sigs);
        (Node = {attribute, _, Modality, _}, Error)
          when Modality =:= ?M_NEW; Modality =:= ?M_USE ->

          % Invalid mailbox spec.
          ?pushError(?E_MB_SPEC_BAD, Node, Error);
        (_, Error) ->
          Error
      end,
      #error{}, Forms)
  ).


%%% ----------------------------------------------------------------------------
%%% Type consistency checking functions.
%%% ----------------------------------------------------------------------------

%% Returns the mailbox spec map relating function signatures to mailbox
%% modalities and names.
%% Also checks that each function signature is associated with at most one
%% mailbox name in either the new or use modality.
%% @returns {ok, MbSpecs, Warnings} or {error, Errors, Warning}. Warnings is
%% always the empty list for this call.
%% TODO: Type specs.
make_mb_defs(MbDefs) when is_list(MbDefs) ->
  {MbSpecs, Error} =
    lists:foldl(
      fun({{Modality, Mailbox}, {ANNO, Sigs}}, {Map, Error}) ->
        lists:foldl(
          fun(Sig = {_, _}, {Map, Error}) ->
            case maps:is_key(Sig, Map) of
              true ->

                % Function signature already associated with other mailbox type.
                Node = to_erl_af(ANNO, Sig),
                {Map, ?pushError(?E_MB_SIG_NOT_UNIQUE, Node, Error)};
              false ->

                % Associate function signature with mailbox type.
                {Map#{Sig => {Modality, ANNO, Mailbox}}, Error}
            end
          end,
          {Map, Error}, Sigs)
      end,
      {#{}, #error{}}, MbDefs),

  return(MbSpecs, Error).

%% @private Checks that the function signatures in mailbox definitions are
%% defined.
%% @returns `{ok, Warnings}` if all signatures are defined, otherwise
%% `{error, Warnings, Errors}`. Warnings is always the empty list.
check_mb_sigs_defined(MbDefs = #{}, Sigs) when is_list(Sigs) ->
  return(
    maps:fold(
      fun(Sig = {_, _}, {ANNO, _, _}, Error) ->
        case lists:member(Sig, Sigs) of
          true ->

            % Function signature in mailbox spec defined.
            Error;
          false ->

            % Undefined function signature in mailbox spec.
            Node = to_erl_af(ANNO, Sig),
            ?pushError(?E_MB_SIG_UNDEF, Node, Error)
        end
      end,
      #error{}, MbDefs)
  ).

%% @private Checks that mailbox names have a corresponding type defined.
%% @returns `{ok, Warnings}` if all mailbox names have a corresponding type
%% defined, otherwise `{error, Errors, Warnings}`. Warnings is always the empty
%% list.
check_mb_types_defined(MbNames = #{}, Types = #{}) ->
  return(
    maps:fold(
      fun(Mailbox, {ANNO}, Error) ->
        case maps:is_key(Mailbox, Types) of
          true ->

            % Mailbox defined as type.
            Error;
          false ->

            % Undefined mailbox type.
%%            Node = revert(set_pos(atom(Mailbox), ANNO)),
            Node = to_erl_af(ANNO, Mailbox),
            ?pushError(?E_MB_TYPE_UNDEF, Node, Error)
        end
      end,
      #error{}, MbNames)
  ).


% TODO: Implementing this.
check_mb_types_valid(Types = #{}) ->
  return(
    maps:fold(
      fun(Name, {?T_MBOX, ANNO, Type, Vars}, Error) ->
        ?TRACE(">> Checking type: ~p = ~p", [Name, Type]),


        case check_msg_type_valid(Type, Types, Error, false) of
          {true, Error0} ->

            ?TRACE("Pid OK"),


            Error0;
          {false, Error0} ->

            % Pid missing.
            ?TRACE("Pid MISSING ~p", [Error0]),

%%            Node = revert(set_pos(atom(Name), ANNO)),
            Node = to_erl_af(ANNO, Name),
            ?pushWarning(?W_MB_NO_PID, Node, Error0)
        end;
        (_, {?T_TYPE, _, _, _}, Error) ->

          % Skip non-mailbox type.
          Error
      end,
      #error{}, Types)
  ).



check_msg_type_valid(T = {type, _, pid, _}, #{}, Error, _) ->
  ?TRACE("Type: ~p", [T]),
  {true, Error};

check_msg_type_valid(T = {type, ANNO, tuple, [Node]}, #{}, Error, HasPid) ->
  ?TRACE("Type: ~p", [T]),

%%  case is_valid_tag(Elem) of
%%    true ->
%%      ?TRACE("~p is a valid tag", [Elem]),
%%      {HasPid, Error};
%%    false ->
%%      ?TRACE("~p is NOT a valid tag", [Elem]),
%%      {HasPid, ?pushError(?E_MB_MSG_TAG_BAD, Elem, Error)}
%%  end;
  {HasPid, check_valid_tag(Node, Error)};



check_msg_type_valid(T = {type, _, tuple, [Node | Nodes]}, Types = #{}, Error, HasPid) ->
  ?TRACE("Type: ~p", [T]),


%%  case is_valid_tag(Elem) of
%%    true ->
%%      ?TRACE("~p is a valid tag", [Elem]),
%%
%%      case is_valid_payload(Elems, Types) of
%%        true ->
%%          ?TRACE("~p is a valid payload", [Elems]),
%%          {HasPid, Error};
%%        false ->
%%          ?TRACE("~p is NOT a valid payload", [Elems]),
%%          {HasPid, Error}
%%      end;
%%    false ->
%%      ?TRACE("~p is NOT a valid tag", [Elem]),
%%      {false, Error}
%%  end;

  case check_valid_tag(Node, Error) of
    #error{errors = []} ->
      {HasPid, check_valid_tuple_nodes(Nodes, Types, Error)};
    Error0 = #error{} ->
      {HasPid, Error0}
  end;


check_msg_type_valid(T = {user_type, ANNO, Type, Vars}, Types = #{}, Error, HasPid) ->
  ?TRACE("USER Type: ~p", [T]),

  % Query types map.
%%  case maps:is_key(Type, Types) of
%%    true ->
%%      {HasPid, Error};
%%    false ->
%%      {HasPid, Error}
%%  end;
  case maps:get(Type, Types, undefined) of
    {_, _, Type0, _} ->

      ?TRACE("The found usertype is ~p", [Type0]),

%%      {A, B} = check_msg_type_valid(Type0, Types, Error, HasPid),
%%      ?TRACE("HasPid in user type? ~p", [A]),

      check_msg_type_valid(Type0, Types, Error, HasPid);
    undefined ->

      % TODO: maybe use an assert.
      % This case is handled by the Erlang preprocessor and should never happen.
      {HasPid, Error}
  end;


check_msg_type_valid(T = {type, _, union, Elems}, Types = #{}, Error, HasPid) ->
  ?TRACE("Type: ~p", [T]),

  check_msgs_types_valid(Elems, Types, Error, HasPid);


check_msg_type_valid(T = {type, ANNO, Type, _}, Types = #{}, Error, HasPid) ->
  ?ERROR("Type: ~p", [T]),
  % Type not permitted.
  {HasPid, Error}.


check_msgs_types_valid([], Types = #{}, Error, HasPid) ->
  {HasPid, Error};
check_msgs_types_valid([Elem | Elems], Types = #{}, Error, HasPid) ->
  {HasPid0, Error0} = check_msg_type_valid(Elem, Types, Error, HasPid),
  check_msgs_types_valid(Elems, Types, Error0, HasPid0).





check_valid_tag(Node = {atom, _, _}, Error) ->
  ?TRACE("~p is a valid tag", [Node]),
  Error;
check_valid_tag(Node, Error) ->
  ?TRACE("~p is NOT a valid tag", [Node]),
  ?pushError(?E_MB_MSG_TAG_BAD, Node, Error).


check_valid_tuple_nodes([], #{}, Error) ->
  Error;
check_valid_tuple_nodes([{type, _, integer, _} | Nodes], Types = #{}, Error) ->
  check_valid_tuple_nodes(Nodes, Types, Error);
check_valid_tuple_nodes([Node = {user_type, ANNO, Type, _} | Nodes], Types = #{}, Error) ->
  case maps:is_key(Type, Types) of
    true ->
      check_valid_tuple_nodes(Nodes, Types, Error);
    false ->

      % TODO: This should never occur because the Erlang preprocessor ensures that types are valid.
      check_valid_tuple_nodes(Nodes, Types, ?pushError(?E_MB_TYPE_UNDEF, Node, Error))
  end;
check_valid_tuple_nodes([Node | Nodes], Types = #{}, Error) ->
  check_valid_tuple_nodes(Nodes, Types, ?pushError(?E_MB_MSG_ELEM_TYPE_BAD, Node, Error)).


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
%% a duncan_mb mailbox scope. This would be in another file for semantic checks.


%%% ----------------------------------------------------------------------------
%%% Error handling and reporting.
%%% ----------------------------------------------------------------------------

format_sig({Fun, Arity}) ->
  io_lib:format("~s/~b", [Fun, Arity]).

%% @doc Formats the specified error to human-readable form.
format_error({?E_MB_SPEC_BAD, Node}) ->
  io_lib:format(
    "bad mailbox spec '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_MB_SIG_BAD, Node}) ->
  io_lib:format(
    "mailbox spec contains bad function signature '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_MB_SIG_NOT_UNIQUE, Node}) ->
  io_lib:format(
    "function signature '~s' associated with more than one mailbox spec",
    [erl_prettypr:format(Node)]
  );
format_error({?E_MB_SIG_UNDEF, Node}) ->
  io_lib:format(
    "mailbox spec contains undefined function signature '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_MB_TYPE_UNDEF, Node}) ->
  io_lib:format(
    "undefined mailbox type '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_MB_MSG_TAG_BAD, Node}) ->
  io_lib:format(
    "bad message tag '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?E_MB_MSG_ELEM_TYPE_BAD, Node}) ->
  io_lib:format(
    "bad message element type '~s'",
    [erl_prettypr:format(Node)]
  );
format_error({?W_MB_NO_PID, Node}) ->
  io_lib:format(
    "mailbox type '~s' does not contain pid() that makes it incompatible with Dialyzer",
    [erl_prettypr:format(Node)]
  ).




to_erl_af(ANNO, Name) when is_atom(Name) ->
  revert(set_pos(atom(Name), ANNO));
to_erl_af(ANNO, {Name, Arity})
  when is_atom(Name), is_integer(Arity) ->
  revert(set_pos(implicit_fun(atom(Name), integer(Arity)), ANNO)).


% TODO: Move to another module, maybe util

%% @doc Formats return values using the Erlang preprocessor pattern.
%% 1. An error-free result with possible warnings returns `{ok, Result, Warnings}`.
%% 2. Otherwise only errors are returned `{error, Errors, Warnings}`.
return(Result, #error{errors = [], warnings = Warnings}) ->
  {ok, Result, Warnings};
return(_, Error = #error{}) ->
  Error.

%% @doc Formats return values using the Erlang preprocessor pattern.
%% 1. No errors and potential warnings returns `{ok, Warnings}`.
%% 2. Errors are returned as `{error, Errors, Warnings}`.
%% 3. Results are returned as `{ok, Result, Warnings}`, where Warnings is the
%% empty list.
return(#error{errors = [], warnings = Warnings}) ->
  {ok, Warnings};
return(Error = #error{}) ->
  Error;
return(Result) ->
  {ok, Result, []}.





