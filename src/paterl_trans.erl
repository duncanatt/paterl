%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% The Pat Erlang source-to-source translator.
%%% @end
%%% Created : 29. Jan 2024 15:22
%%%-------------------------------------------------------------------
-module(paterl_trans).
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

-define(MB_IDX_START, 0).

-define(SEP_PAT, [$,, $\s]).

-define(SEP_NL, [$\n]).

-spec translate(erl_syntax:forms()) -> list().
translate(Forms) ->
  translate_forms(Forms).




translate_forms([]) ->
  ["\n"];
translate_forms([Form | Forms]) ->
  [translate_form(Form) | translate_forms(Forms)].

%%% ----------------------------------------------------------------------------
%%% Translation on open terms.
%%% ----------------------------------------------------------------------------



translate_form({attribute, Anno, module, Name}) ->
  ["# Translated from ", atom_to_list(Name), ".erl\n"];
translate_form({attribute, Anno, interface, {Name, Type, _Vars = []}}) ->
  Type0 = translate_type(Type),
  ["interface ", string:titlecase(atom_to_list(Name)), " {\n", Type0, "\n}\n\n"];

translate_form({function, Anno, Name, Arity, Clauses = [_]}) ->
  % Function with one clause.
  Clauses0 = translate_clauses(Clauses),
  ["def ", atom_to_list(Name), Clauses0, "\n"];
translate_form(_) ->
  % Other module attributes without Pat equivalent.
  [].


translate_type({type, _, pid, _Vars = []}) ->
  % PID type is not translated.
  "";
translate_type({type, _, integer, _Vars = []}) ->
  "Int";
translate_type({type, _, string, _Vars = []}) ->
  "String";
translate_type({user_type, _, Name, _Vars = []}) ->
  [string:titlecase(atom_to_list(Name)), "!"];
translate_type({type, _, tuple, [{atom, _, Name} | TypeSeq]}) ->
  % Message signature type.
  Msg = string:join(translate_type_seq(TypeSeq), ","),
  ["\t", string:titlecase(atom_to_list(Name)), "(", Msg, ")"];
translate_type({type, _, union, TypeSeq}) when is_list(TypeSeq) ->
  % Union of message types
  string:join(translate_type_seq(TypeSeq), ",\n").

translate_type_seq([]) ->
  [];
translate_type_seq([{type, _, pid, _Vars = []} | TypeSeq]) ->
  translate_type_seq(TypeSeq);
translate_type_seq([Type | TypeSeq]) ->
  [translate_type(Type) | translate_type_seq(TypeSeq)].


%%% ----------------------------------------------------------------------------
%%% Translation on open terms.
%%% ----------------------------------------------------------------------------

%% @private Translates receive/case and if clauses.
%%translate_clauses(Clauses, MbCtx) ->
%%  ?TRACE("(~s) Translating open clauses.", [MbCtx]),
%%  {X, Y} =
%%  lists:foldl(
%%    fun(Clause, {Clauses0, MbCtx0}) ->
%%      {Clause1, MbCtx1} = translate_clause(Clause, MbCtx0),
%%      {[Clause1 | Clauses0], MbCtx1}
%%    end,
%%    {[], MbCtx}, Clauses
%%  ),
%%  ?TRACE("(~s) Translated clauses: ~p", [Y, X]),
%%  {lists:reverse(X), Y}.
translate_clauses(Clauses, MbCtx) ->
  {Clauses0, MbCtx0} =
    lists:foldl(
      fun(Clause, {Clauses0, MbCtx0}) ->
        {Clause1, MbCtx1} = translate_clause(Clause, MbCtx0),
        {[Clause1 | Clauses0], MbCtx1}
      end,
      {[], MbCtx}, Clauses
    ),
  {lists:reverse(Clauses0), MbCtx0}.

%% @private Translates a receive/case and if clause.
translate_clause(_Clause = {clause, Anno, PatSeq = [_], _GuardSeq = [], Body}, MbCtx) ->
  ?TRACE("Translating receive clause: ~p", [_Clause]),
  % Unguarded receive/case clause.
%%  MbCtx0 = new_mb(MbCtx),
  MbCtx0 = new_mb(),

  PatSeq0 = translate_pat_seq(PatSeq),
  {Body0, MbCtx1} = translate_body(Body, MbCtx0),
  Expr1 = io_lib:format("receive ~s from ~s ->~n~s", [
    PatSeq0, MbCtx1, Body0
  ]),

  {Expr1, MbCtx1};

translate_clause(_Clause = {clause, Anno, _PatSeq = [], GuardSeq = [_], Body}, MbCtx) ->
  % Unguarded if clause.
  ?TRACE("(~p) About to translate if clause: ~p", [MbCtx, _Clause]),
  {Body0, MbCtx0} = translate_body(Body, MbCtx),
  ?TRACE("(~p) Translated body of if clause: ~p", [MbCtx0, Body0]),

  Expr1 =
    case translate_guard_seq(GuardSeq) of
      [["true"]] ->
        % Default Erlang if guard equates to else branch in Pat.
        io_lib:format("{~n~s~n}", [Body0]);
      GuardSeq0 ->
        % If branch.
        io_lib:format("(~s) {~n~s~n}~n", [GuardSeq0, Body0])
    end,

  {Expr1, MbCtx0}.


translate_body(ExprSeq, MbCtx) ->
  {ExprSeq0, MbCtx0} = translate_expr_seq(ExprSeq, MbCtx),
  {string:join(ExprSeq0, ?SEP_NL), MbCtx0}.

translate_args(ExprSeq, MbCtx) -> % TODO: Not needed and for completeness.
  {ExprSeq0, MbCtx0} = translate_expr_seq(ExprSeq, MbCtx),
  {string:join(ExprSeq0, ?SEP_PAT), MbCtx0}.

translate_expr_seq(ExprSeq, MbCtx) ->
  ?TRACE("(~s) Translating open expression sequence.", [MbCtx]),
  {ExprSeq0, MbCtx0} =
    lists:foldl(
      fun(Expr, {ExprSeq0, MbCtx0}) ->
        {Expr1, MbCtx1} = translate_expr(Expr, MbCtx0),
        {[Expr1 | ExprSeq0], MbCtx1}
      end,
      {[], MbCtx}, ExprSeq
    ),
  {lists:reverse(ExprSeq0), MbCtx0}.

%% @private Translates values and expressions.
translate_expr({call, _, Self = {atom, _, self}, _MFArgs = []}, MbCtx) ->
  % Self.
  Expr0 = io_lib:format("(~s, ~s)", [MbCtx, MbCtx]),
%%  MbCtx0 = new_mb(MbCtx),
  MbCtx0 = new_mb(),
  {Expr0, MbCtx0};
%%  HERE: Start populating self and the other functions!

translate_expr(Expr = {call, Anno, Fun = {atom, _, Name}, Args}, MbCtx) ->
  % Explicit internal function call and explicit internal mailbox-annotated
  % function call.

  Expr1 =
    case paterl_anno:interface(Anno) of
      undefined ->
        io:format("-------------------------------------------------------> We are translating function in context but it is closed", []),
        % Call to closed function call outside mailbox context.
        io_lib:format("(~s, ~s)", [translate_expr(Expr), MbCtx]);
      Interface ->
        % Call to an open function call inside mailbox context.
        case paterl_anno:modality(Anno) of
          new ->
            io:format("-------------------------------------------------------> We are translating a NEW function in context", []),
            % Inject new mailbox.
            io_lib:format("(~s, ~s)", [translate_expr(Expr), MbCtx]);
          use ->
            io:format("-------------------------------------------------------> We are translating a USE function in context ~p", [Expr]),
            % Thread through existing mailbox.
            Args0 = [erl_syntax:revert(erl_syntax:atom(MbCtx)) | Args],
            io_lib:format("~s(~s)", [
              Name, translate_args(Args0)
            ])
        end
    end,
  {Expr1, new_mb()};
translate_expr({match, _, Pat, Expr}, MbCtx) ->
  % Match.
  {Expr0, MbCtx0} = translate_expr(Expr, MbCtx),

  Expr1 = io_lib:format("let (~s, ~s) =~n~s~nin", [
    translate_pat(Pat), MbCtx0, Expr0
  ]),
%%  MbCtx1 = new_mb(MbCtx0),
  MbCtx1 = new_mb(),
  {Expr1, MbCtx1};
translate_expr({'if', _, Clauses = [_, _]}, MbCtx) ->
  % If else.
  {[Clause0, Clause1], MbCtx0} = translate_clauses(Clauses, MbCtx),
  ?TRACE("Translated clauses: ~p and ~p", [Clause0, Clause1]),

  Expr1 = io_lib:format("if ~selse ~s", [
    Clause0, Clause1
  ]),
%%  MbCtx1 = new_mb(MbCtx0),
  MbCtx1 = new_mb(),

%%  {["if ", Clause0, "else ", Clause1], MbCtx0};
  {Expr1, MbCtx1};
translate_expr({'receive', Anno, Clauses}, MbCtx) ->
  % Receive.
  State = paterl_anno:state(Anno),
  {Clauses0, MbCtx0} = translate_clauses(Clauses, MbCtx),


  % TODO: Logic to determine when to free.

  Expr0 = io_lib:format("guard ~s: ~s {~n~s~n}~n", [
    MbCtx0, State, Clauses0
  ]),

  Expr1 =
    case is_mb_empty(State) of
      true ->
        Expr0 ++ lists:flatten(
          io_lib:format("empty(~s)->~n((), ~s)", [MbCtx0, MbCtx0])
        );
      false ->
        Expr0
    end,

%%  MbCtx1 = new_mb(MbCtx0),
  MbCtx1 = new_mb(),
  {Expr1, MbCtx1};
translate_expr(Expr, MbCtx) ->
  % Literal.
  % Variable.
  % Unguarded function call to closed expression.
  % Binary operator.
  % Unary operator.
  % Spawn.
  Expr1 = io_lib:format("(~s, ~s)", [translate_expr(Expr), MbCtx]),
  {Expr1, MbCtx}.


%%% ----------------------------------------------------------------------------
%%% Translation on closed terms.
%%% ----------------------------------------------------------------------------

%% @private Translates closed functions and if clauses.
translate_clauses(Clauses) ->
  [translate_clause(Clause) || Clause <- Clauses].

%% @private Translates closed function and if clauses.
translate_clause(_Clause = {clause, Anno, PatSeq, _GuardSeq = [], Body}) ->
  % Unguarded function clause.

  % New mailbox.
  MbCtx0 = new_mb(),
%%  Params = translate_pat_seq(PatSeq),

%%  Params = translate_params([PatSeq]),


  MbPat = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:atom(MbCtx0),
      paterl_anno:set_type(paterl_anno:interface(Anno), Anno)
    )),

  Params = translate_params([MbPat | PatSeq]),
  RetType = erl_to_pat_type(paterl_anno:type(Anno)),
  case paterl_anno:interface(Anno) of
    undefined ->
      % Non mailbox-annotated function.
      ?TRACE("Translating unguarded NON mailbox-annotated function clause: ~p", [_Clause]),

      Body0 = translate_body(Body),
      io_lib:format("(~s): ~s {~n~s~n}~n", [Params, RetType, Body0]);
    Interface ->
      % Mailbox-annotated function.
      ?TRACE("Translating unguarded mailbox-annotated function clause: ~p", [_Clause]),

      Interface0 = erl_to_pat_type(Interface),

      {Body0, _} = translate_body(Body, MbCtx0),
      io_lib:format("(~s): (~s * ~s) {~n~s~n}~n", [Params, RetType, Interface0, Body0])
  end;

translate_clause({clause, _, _PatSeq = [], GuardSeq = [_], Body}) ->
  % Guarded if clause.
  Body0 = translate_body(Body),
  case translate_guard_seq(GuardSeq) of
    [["true"]] ->
      % Default Erlang if guard equates to else branch in Pat.
      io_lib:format("{~n~s~n}", [Body0]);
    GuardSeq0 ->
      % If branch.
      io_lib:format("(~s) {~n~s~n}~n", [GuardSeq0, Body0])
  end.

translate_body(ExprSeq) ->
  string:join(translate_expr_seq(ExprSeq), ?SEP_NL).
translate_args(ExprSeq) ->
  string:join(translate_expr_seq(ExprSeq), ?SEP_PAT).

%% @private Translates value and expression sequences.
translate_expr_seq(ExprSeq) ->
  ?TRACE("Have to traslate expr seq: ~p", [ExprSeq]),
  Trans = [translate_expr(Expr) || Expr <- ExprSeq],
  ?TRACE("Translated expr_seq: ~p", [Trans]),
  io:format("TRANSLATED: ~s", [(Trans)]),
%%  string:join([translate_expr(Expr) || Expr <- ExprSeq], ?SEP_NL).
%%  string:join(Trans, ?SEP_NL).
  Trans.

%% @private Translates values and expressions.
translate_expr(Lit)
  when
% Literal.
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  translate_lit(Lit);
translate_expr({var, _, Name}) ->
  % Variable.
  string:lowercase(atom_to_list(Name));
translate_expr({tuple, _, [_Tag = {atom, _, Name} | Payload]}) ->
  % Message.
  Msg = string:titlecase(atom_to_list(Name)),
  Args = translate_args(Payload),
  io_lib:format("~s(~s)", [Msg, Args]);


%%translate_expr(Expr = {call, _, {atom, _, spawn}, }) ->
translate_expr(Expr = {call, Anno, Spawn = {atom, _, spawn}, _MFArgs = [_, Fun, Args]}) ->
  % Spawn.
  Interface = string:titlecase(atom_to_list(paterl_anno:interface(Anno))),


  % TODO: Need to translate arguments.
  % TODO: We need to use a tuple and let.

%%  let Y =~n
%%    new[Interface]~n
%%  in~n
%%    let y =
%%      spawn { let (x, YPrime) = TransFunCall_Mb in free(YPrime); x}
%%    in Y


%%  TODO: I need to build an expression that is equal to Fun(Args).


  Args0 = erl_syntax:list_elements(Args),
  Expr0 = erl_syntax:revert(
    erl_syntax:set_pos(
      erl_syntax:application(Fun, Args0), paterl_anno:set_modality(use, Anno))
  ),



%%  Anno0 = paterl_anno:set_modality(use, Anno),
%%  Expr0 = paterl_anno:map_anno(fun(_) -> Anno0 end, Expr),

%%  io:format("=====================================> FunApplication: ~p~n", [Expr0]),

  MbCtx0 = new_mb(),
  MbCtx1 = new_mb(),

  {Expr1, _} = translate_expr(Expr0, MbCtx0),

  Expr2 =
    lists:flatten(
      io_lib:format("spawn { let (x, ~s) = ~s in free(~s); x }", [
        MbCtx1, Expr1, MbCtx1
      ])
    ),
  io_lib:format("let ~s =~nnew[~s]~nin~nlet y =~n~s~nin ~s", [
    MbCtx0, Interface, Expr2, MbCtx0
  ]);

%%  ["TODO: spawn"];
translate_expr({call, _, {atom, _, format}, Args}) ->
  Args0 = translate_args(Args),
  io_lib:format("print(~s)", [Args0]);
%%  "format!";
translate_expr({call, Anno, Fun = {atom, _, Name}, Args}) ->
  % Explicit internal function call and explicit internal mailbox-annotated
  % function call.
  % TODO: Need to translate arguments.
  % TODO: We need to use a tuple and let.

%%  ?TRACE("====> Found interface ~p~n", []),

  case paterl_anno:modality(Anno) of
    undefined ->
      % Call to closed function call outside mailbox context.
      Args0 = translate_args(Args),
      ?TRACE("Translated function arguments: ~p", [lists:flatten(Args0)]),
      io_lib:format("~s(~s)", [Name, Args0]);
    new ->
      % Only the new interface modality is expected at this point.
      Interface = string:titlecase(atom_to_list(paterl_anno:interface(Anno))),

      MbCtx0 = new_mb(),
      MbCtx1 = new_mb(),

      Expr0 = erl_syntax:revert(
        erl_syntax:set_pos(
          erl_syntax:application(Fun, Args), paterl_anno:set_modality(use, Anno))
      ),

      {Expr1, _} = translate_expr(Expr0, MbCtx0),

      io_lib:format("let ~s =
              new[~s]
            in
              let (x, ~s) =
                ~s
              in
                let y = free(~s) in x",
        [MbCtx0, Interface, MbCtx1, Expr1, MbCtx1])
  end;



%%  Args = translate_args(MFArgs),
%%  io_lib:format("~s(~s)", [Name, Args]);
translate_expr({match, Anno, Pat, Expr}) ->
  % Match.
  io_lib:format("let ~s = ~s~nin~n", [translate_pat(Pat), translate_expr(Expr)]);
translate_expr({op, Anno, Op, Expr0, Expr1}) ->
  % Binary operator.
  Expr2 = translate_expr(Expr0),
  Expr3 = translate_expr(Expr1),
  io_lib:format("~s ~s ~s", [Expr2, Op, Expr3]);
translate_expr({op, _, Op, Expr}) ->
  % Unary operator.
  Expr0 = translate_expr(Expr),
  io_lib:format("~s ~s", [Op, Expr0]);
translate_expr({'if', _, Clauses = [_, _]}) ->
  % If else.
  [Clause0, Clause1] = translate_clauses(Clauses),
  io_lib:format("if ~selse ~s", [Clause0, Clause1]);
%%translate_expr({cons, _, Expr0, Expr1}) ->
%%  % TODO: Check these list things again because I'm not sure.
%%  % TODO: Have a function, translate_print, that translate each literal value
%%  % TODO: by wrapping it up in integerToStr, floatToStr, etc.
%%  [translate_expr(Expr0) | translate_expr(Expr1)];
%%translate_expr({nil, _}) ->
%%  % TODO: Check these list things again because I'm not sure.
%%  "NIL";
translate_expr(Other) ->
  ?ERROR("Cannot translate: ~p", [Other]), "error".


%%% ----------------------------------------------------------------------------
%%% Translation on guards and patterns.
%%% ----------------------------------------------------------------------------

%% @private Translates a guard sequence.
translate_guard_seq(GuardSeq) ->
  [translate_guard(Guard) || Guard <- GuardSeq].

%% @private Translates a guard, which is a sequence of guard tests.
translate_guard(GuardTests) ->
  [translate_guard_test(GuardTest) || GuardTest <- GuardTests].

%% @private Translates a guard test.
translate_guard_test(Lit)
  when
% Literal.
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  translate_lit(Lit);
translate_guard_test({var, _, Name}) ->
  % Variable.
  string:lowercase(atom_to_list(Name));
translate_guard_test({call, _, {atom, _, Name}, GuardTests}) ->
  % Function call.
  % TODO: I think the arguments need to be translated as expressions and not guard tests.
  GuardTests0 = string:join(translate_guard(GuardTests), ","),
  [atom_to_list(Name), "(", GuardTests0, ")"];
translate_guard_test({op, _, Op, GuardTest0, GuardTest1}) ->
  % Binary operator.
  GuardTest2 = translate_guard_test(GuardTest0),
  GuardTest3 = translate_guard_test(GuardTest1),
  [GuardTest2, " ", atom_to_list(Op), " ", GuardTest3];
translate_guard_test({op, _, Op, GuardTest}) ->
  % Unary operator.
  GuardTest0 = translate_guard_test(GuardTest),
  [atom_to_list(Op), GuardTest0].

translate_params(PatSeq) ->
  Translate = fun(Pat) ->
    Type = erl_to_pat_type(paterl_anno:type(element(2, Pat))),
    io_lib:format("~s: ~s", [translate_pat(Pat), Type])
              end,
  string:join([Translate(Pat) || Pat <- PatSeq], ?SEP_PAT).


%% @private Translates pattern sequences.
translate_pat_seq(PatSeq) ->
  string:join([translate_pat(Pat) || Pat <- PatSeq], ?SEP_PAT).

%% @private Translates patterns.
translate_pat(Lit)
  when
% Literal.
  element(1, Lit) =:= integer;
  element(1, Lit) =:= float;
  element(1, Lit) =:= string;
  element(1, Lit) =:= atom ->
  translate_lit(Lit);
translate_pat({var, _, Name}) ->
  % Variable.
  string:lowercase(atom_to_list(Name));
translate_pat({tuple, _, [_Tag = {atom, _, Name} | Payload]}) ->
  % Message.
  Msg = string:titlecase(atom_to_list(Name)),
  Args = translate_pat_seq(Payload),
  io_lib:format("~s(~s)", [Msg, Args]).

%% @private Translates literals.
translate_lit({integer, _, Value}) ->
  integer_to_list(Value);
translate_lit({float, _, Value}) ->
  float_to_list(Value);
translate_lit({string, _, Value}) ->
  Value;
translate_lit({atom, _, Value}) ->
  atom_to_list(Value).


%%% ----------------------------------------------------------------------------
%%% Helpers.
%%% ----------------------------------------------------------------------------

%%make_mb(MbId) ->
%%  "mb" + integer_to_list(MbId).

%%new_mb1(MbId) ->
%%  {"mb" ++ integer_to_list(MbId), MbId + 1}.

%%new_mb() ->
%%  "mb" ++ integer_to_list(?MB_IDX_START).
%%
%%new_mb([$m, $b | IdStr]) ->
%%  "mb" ++ integer_to_list(list_to_integer(IdStr) + 1).

new_mb() ->
  "mb" ++
  integer_to_list(
    case get(mb) of
      undefined ->
        put(mb, 0),
        0;
      Mb ->
        put(mb, Mb + 1),
        Mb
    end
  ).

%% @private Checks whether a mailbox can become potentially empty.
is_mb_empty(Regex) ->
  case re:run(Regex, "^1|\\*.*|1.*|.*1$") of
    {match, _} ->
      true;
    _ ->
      false
  end.
%%HERE! I need to integrate this with the translation of guard above.


simplify([]) ->
  ok.

erl_to_pat_type(integer) ->
  "Int";
erl_to_pat_type(float) ->
  "Float";
erl_to_pat_type(string) ->
  "String";
erl_to_pat_type(none) ->
  "Unit";
erl_to_pat_type(no_return) ->
  "Unit";
erl_to_pat_type(ok) ->
  "Unit";
erl_to_pat_type(any) ->
  "Unit";
erl_to_pat_type(Mb) when is_atom(Mb) ->
  string:titlecase(atom_to_list(Mb)) ++ "?";
erl_to_pat_type(_) ->
  "Unknown".

