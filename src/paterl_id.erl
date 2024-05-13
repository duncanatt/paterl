%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. May 2024 11:10
%%%-------------------------------------------------------------------
-module(paterl_id).
-author("duncan").

%% API
-export([id_forms/1]).


%%% ----------------------------------------------------------------------------
%%% Module declarations and forms.
%%% ----------------------------------------------------------------------------

%% @doc Module declaration.
id_forms(Forms) ->
  [id_form(Form) || Form <- Forms].

%% @private Forms.
id_form({attribute, Anno, export, Sigs}) when is_list(Sigs) ->
  % Export attribute.
  {attribute, Anno, export, id_sigs(Sigs)};
id_form({attribute, Anno, import, {Mod, Sigs}}) when is_list(Sigs) ->
  % Import attribute.
  {attribute, Anno, import, {Mod, id_sigs(Sigs)}};
id_form({attribute, Anno, module, Mod}) ->
  % Module attribute.
  {attribute, Anno, module, Mod};
id_form({attribute, Anno, file, {File, Line}}) ->
  % File attribute.
  {attribute, Anno, file, {File, Line}};
id_form({function, Anno, Name, Arity, Clauses}) ->
  % Function declaration.
  {function, Anno, Name, Arity, id_clauses(Clauses)};
id_form({attribute, Anno, Spec, {{Name, Arity}, TypeSeq}})
  when Spec =:= spec; Spec =:= callback, is_list(TypeSeq) ->
  % Function specification or callback.
  {attribute, Anno, Spec, {{Name, Arity}, id_type_seq(TypeSeq)}};
id_form({attribute, Anno, spec, {{Mod, Name, Arity}, TypeSeq}})
  when
  is_list(TypeSeq)->
  % Function specification.
  {attribute, Anno, spec, {{Mod, Name, Arity}, id_type_seq(TypeSeq)}};
id_form({attribute, Anno, record, {Name, Fields}}) ->
  % Record declaration.
  {attribute, Anno, record, {Name, id_fields(Fields)}};
id_form({attribute, Anno, Type, {Name, Type, Vars}})
  when
  Type =:= type, is_list(Vars);
  Type =:= opaque, is_list(Vars) ->
  % Type declaration.
  {attribute, Anno, Type, {Name, Type, id_type_seq(Vars)}};
id_form({attribute, Anno, A, T}) ->
  % Wild attribute.
  {attribute, Anno, A, T};
id_form({error, E}) ->
  % epp or erl_parse error.
  {error, E};
id_form({warning, W}) ->
  % epp or erl_parse warning.
  {warning, W};
id_form({eof, Location}) ->
  % EOF marker.
  {eof, Location}.

%% @private Function signatures.
id_sigs(Sigs) ->
  [id_sig(Sig) || Sig <- Sigs].

id_sig(Sig = {_Name, _Arity}) ->
  Sig.

%% @private Record fields.
id_fields(Fields) ->
  [id_field(Field) || Field <- Fields].

id_field({record_field, Atom, Assoc}) ->
  % Association.
  {record_field, Atom, id_assoc(Assoc)};
id_field({record_field, Anno, Assoc, Expr}) ->
  % Association with expression.
  {record_field, Anno, id_assoc(Assoc), id_expr(Expr)};
id_field({typed_record_field, {record_field, Anno, Assoc}, Type}) ->
  % Typed association.
  {typed_record_field, {record_field, Anno, id_assoc(Assoc)}, id_type(Type)};
id_field({typed_record_field, {record_field, Anno, Assoc, Expr}, Type}) ->
  % Typed association with expression.
  {typed_record_field,
%%    {record_field, Anno, id_assoc(Assoc), id_expr(Expr)}, id_type(Type)
    {record_field, Anno, id_lit(Assoc), id_expr(Expr)}, id_type(Type)
  }.

%%{record_field, Anno, id_lit(Field), id_pat(Pat)}

%%% ----------------------------------------------------------------------------
%%% Atomic literals.
%%% ----------------------------------------------------------------------------

%% @private Atomic literals.
id_lit({atom, Anno, Value}) ->
  {atom, Anno, Value};
id_lit({char, Anno, Value}) ->
  {char, Anno, Value};
id_lit({float, Anno, Value}) ->
  {float, Anno, Value};
id_lit({integer, Anno, Value}) ->
  {integer, Anno, Value};
id_lit({string, Anno, Value}) ->
  {string, Anno, Value}.


%%% ----------------------------------------------------------------------------
%%% Patterns.
%%% ----------------------------------------------------------------------------

id_pat_seq(PatSeq) ->
  [id_pat(Pat) || Pat <- PatSeq].

id_pat(Pat) when
  element(1, Pat) =:= atom;
  element(1, Pat) =:= char;
  element(1, Pat) =:= float;
  element(1, Pat) =:= integer;
  element(1, Pat) =:= string ->
  % Literal pattern.
  id_lit(Pat);
id_pat({bin, Anno, BinElems}) ->
  % Bitstring pattern.
  {bin, Anno, id_bin_elem_pat_seq(BinElems)};
id_pat({match, Anno, Pat0, Pat1}) ->
  % Compound pattern.
  {match, Anno, id_pat(Pat0), id_pat(Pat1)};
id_pat({cons, Anno, PatH, PatT}) ->
  % Cons pattern.
  {cons, Anno, id_pat(PatH), id_pat(PatT)};
id_pat({map, Anno, Assocs}) ->
  % Map pattern.
  {map, Anno, id_assoc_seq(Assocs)};
id_pat({nil, Anno}) ->
  % Nil pattern.
  {nil, Anno};
id_pat({op, Anno, Op, Pat0, Pat1}) ->
  % Binary operator pattern.
  {op, Anno, Op, id_pat(Pat0), id_pat(Pat1)};

id_pat({op, Anno, Op, Pat0}) ->
  % Unary operator pattern.
  {op, Anno, Op, id_pat(Pat0)};

id_pat({record_index, Anno, Name, Field}) when is_atom(Field) ->
  % Record index field pattern.
  {record_index, Anno, Name, id_lit(Field)};

id_pat({record, Anno, Name, Fields}) when is_list(Fields) ->
  % Record pattern.
  {record, Anno, Name, id_field_pat_seq(Fields)};

id_pat({tuple, Anno, PatSeq}) when is_list(PatSeq) ->
  % Tuple pattern.
  {tuple, Anno, id_pat_seq(PatSeq)};

id_pat({var, Anno, '_'}) ->
  % Universal pattern.
  {var, Anno, '_'};

id_pat({var, Anno, Name}) ->
  % Variable pattern.
  {var, Anno, Name}.

%% @private Binary element pattern sequence.
id_bin_elem_pat_seq(PatSeq) ->
  [id_bin_elem_pat(Pat) || Pat <- PatSeq].

%% @private Binary element patterns.
id_bin_elem_pat({bin_element, Anno, Pat, Size, TSL}) ->
  {bin_element, Anno, id_pat(Pat), Size, TSL}.

%% @private Record field pattern sequence.
id_field_pat_seq(PatSeq) ->
  [id_field_pat(Pat) || Pat <- PatSeq].

%% @private Record field patterns.
id_field_pat({record_field, Anno, Field, Pat}) when
  Field =:= '_'; is_atom(Field) ->
  Field0 = if Field =:= '_' -> Field; true -> id_lit(Field) end,
  {record_field, Anno, Field0, id_pat(Pat)}.


%%% ----------------------------------------------------------------------------
%%% Expressions.
%%% ----------------------------------------------------------------------------

%% @private Body.
id_expr_seq(Body) ->
  [id_expr(Expr) || Expr <- Body].

%% @private Expressions.
id_expr(Expr) when
  element(1, Expr) =:= atom;
  element(1, Expr) =:= char;
  element(1, Expr) =:= float;
  element(1, Expr) =:= integer;
  element(1, Expr) =:= string ->
  % Literal expression.
  id_lit(Expr);

id_expr({bc, Anno, Expr, QualSeq}) when is_list(QualSeq) ->
  % Bitstring comprehension expression.
  {bc, Anno, id_expr(Expr), id_qual_seq(QualSeq)};

id_expr({bin, Anno, BinElemExprSeq}) when is_list(BinElemExprSeq) ->
  % Bitstring constructor expression.
  {bin, Anno, id_bin_elem_expr_seq(BinElemExprSeq)};

id_expr({block, Anno, Body}) ->
  % Block expression.
  {block, Anno, id_expr_seq(Body)};

id_expr({'case', Anno, Expr, Clauses}) when is_list(Clauses) ->
  % Case expression.
  {'case', Anno, id_expr(Expr), id_clauses(Clauses)};

id_expr({'catch', Anno, Expr}) ->
  % Catch expression.
  {'catch', Anno, id_expr(Expr)};

id_expr({cons, Anno, ExprH, ExprT}) ->
  % Cons skeleton expression.
  {cons, Anno, id_expr(ExprH), id_expr(ExprT)};

id_expr({'fun', Anno, {function, Name, Arity}}) ->
  % Internal fun reference expression.
  {'fun', Anno, {function, Name, Arity}};

id_expr({'fun', Anno, {function, Module, Name, Arity}}) ->
  % External fun reference expression.
  {'fun', Anno, {function, id_expr(Module), id_expr(Name), id_expr(Arity)}};

id_expr({'fun', Anno, {clauses, Clauses}}) when is_list(Clauses) ->
  % Anonymous fun expression.
  {'fun', Anno, {clauses, id_clauses(Clauses)}};

id_expr({named_fun, Anno, Name, Clauses}) when is_list(Clauses) ->
  % Named fun expression.
  {named_fun, Anno, Name, id_clauses(Clauses)};

id_expr({call, Anno, ExprF, ExprSeq}) when is_list(ExprSeq) ->
% Internal function call expression.
  {call, Anno, id_expr(ExprF), id_expr_seq(ExprSeq)};

id_expr({call, Anno0, {remote, Anno1, ExprM, ExprF}, ExprSeq}) when is_list(ExprSeq) ->
  % External function call expression.
  {call, Anno0, {remote, Anno1, id_expr(ExprM), id_expr(ExprF)}, ExprSeq};

id_expr({'if', Anno, Clauses}) when is_list(Clauses) ->
  % If expression.
  {'if', Anno, id_clauses(Clauses)};

id_expr({lc, Anno, Expr, QualSeq}) when is_list(QualSeq) ->
  % List comprehension expression.
  {lc, Anno, id_expr(Expr), id_qual_seq(QualSeq)};

id_expr({mc, Anno, Expr, QualSeq}) when is_list(QualSeq) ->
  % Map comprehension expression.
  {mc, Anno, id_expr(Expr), id_qual_seq(QualSeq)};

id_expr({map, Anno, AssocSeq}) when is_list(AssocSeq) ->
  % Map creation expression.
  {map, Anno, id_assoc_seq(AssocSeq)};

id_expr({map, Anno, Expr, AssocSeq}) when is_list(AssocSeq) ->
  % Map update expression.
  {map, Anno, id_expr(Expr), id_assoc_seq(AssocSeq)};

id_expr({match, Anno, Pat, Expr}) ->
  % Match expression.
  {match, Anno, id_pat(Pat), id_expr(Expr)};

id_expr({maybe_match, Anno, Pat, Expr}) ->
  % Conditional match operator expression.
  {maybe_match, Anno, id_pat(Pat), id_expr(Expr)};

id_expr({'maybe', Anno, Body}) ->
  % Maybe expression.
  {'maybe', Anno, id_expr_seq(Body)};

id_expr({'maybe', Anno0, Body, {'else', Anno1, Clauses}}) when is_list(Clauses) ->
  % Maybe else expression.
  {'maybe', Anno0, id_expr_seq(Body), {'else', Anno1, id_clauses(Clauses)}};

id_expr({nil, Anno}) ->
  % Nil expression.
  {nil, Anno};

id_expr({op, Anno, Op, Expr0, Expr1}) ->
  % Binary operator expression.
  {op, Anno, Op, id_expr(Expr0), id_expr(Expr1)};

id_expr({op, Anno, Op, Expr}) ->
  % Unary operator expression.
  {op, Anno, Op, id_expr(Expr)};

id_expr({'receive', Anno, Clauses}) when is_list(Clauses) ->
  % Receive expression.
  {'receive', Anno, id_clauses(Clauses)};

id_expr({'receive', Anno, Clauses, Expr, Body}) when is_list(Clauses) ->
  % Receive after expression.
  {'receive', Anno, id_clauses(Clauses), id_expr(Expr), id_expr_seq(Body)};

id_expr({record, Anno, Name, Fields}) when is_list(Fields) ->
  % Record creation expression.
  {record, Anno, Name, id_field_expr_seq(Fields)};

id_expr({record_field, Anno, Expr, Name, Field}) when is_atom(Field) ->
  % Record field access expression.
  {record_field, Anno, id_expr(Expr), Name, Field};

id_expr({record_index, Anno, Name, Field}) when is_atom(Field) ->
  % Record field index expression.
  {record_index, Anno, Name, Field};

id_expr({record, Anno, Expr, Name, Fields}) ->
  % Record update expression.
  {record, Anno, id_expr(Expr), Name, id_field_expr_seq(Fields)};

id_expr({tuple, Anno, ExprSeq}) when is_list(ExprSeq) ->
  % Tuple skeleton expression.
  {tuple, Anno, id_expr_seq(ExprSeq)};

id_expr({'try', Anno, Body, [], Clauses, []})
  when
  is_list(Body), is_list(Clauses) ->
  % Try catch expression.
  {'try', Anno, id_expr_seq(Body), [], id_clauses(Clauses), []};

id_expr({'try', Anno, Body, ClausesC, ClausesT, []})
  when
  is_list(Body), is_list(ClausesC), is_list(ClausesT) ->
  % Try of catch expression.
  {'try', Anno,
    id_expr_seq(Body), id_clauses(ClausesC), id_clauses(ClausesT), []
  };

id_expr({'try', Anno, Body, [], [], After})
  when
  is_list(Body), is_list(After) ->
  % Try after expression. A and B are bodies.
  {'try', Anno, id_expr_seq(Body), [], [], id_expr_seq(After)};

id_expr({'try', Anno, Body, ClausesC, [], After})
  when
  is_list(Body), is_list(ClausesC), is_list(After) ->
  % Try of after expression. A and B are bodies.
  {'try', Anno,
    id_expr_seq(Body), id_clauses(ClausesC), [], id_expr_seq(After)
  };

id_expr({'try', Anno, Body, [], ClausesT, After})
  when
  is_list(Body), is_list(ClausesT), is_list(After) ->
  % Try catch after expression.
  {'try', Anno,
    id_expr_seq(Body), [], id_clauses(ClausesT), id_expr_seq(After)
  };

id_expr({'try', Anno, Body, ClausesC, ClausesT, After})
  when
  is_list(Body), is_list(ClausesC), is_list(ClausesT), is_list(After) ->
  % Try of catch after expression.
  {'try', Anno,
    id_expr_seq(Body), id_clauses(ClausesC),
    id_clauses(ClausesT), id_expr_seq(After)
  };

id_expr({var, Anno, Name}) ->
  % Variable expression.
  {var, Anno, Name}.




id_bin_elem_expr_seq(ExprSeq) ->
  [id_bin_elem_expr(Expr) || Expr <- ExprSeq].

id_bin_elem_expr({bin_element, Anno, Expr, Size, TSL}) ->
  {bin_element, Anno, id_expr(Expr), id_expr(Size), TSL}.


%% @private Qualifier sequence.
id_qual_seq(QualSeq) ->
  [id_qual(Qual) || Qual <- QualSeq].

%% @private Qualifiers.
id_qual(Expr) ->
  % Filter.
  id_expr(Expr);
id_qual({generate, Anno, Pat, Expr}) ->
  % List generator.
  {generate, Anno, id_pat(Pat), id_expr(Expr)};
id_qual({b_generate, Anno, Pat, Expr}) ->
  % Bitstring generator.
  {b_generate, Anno, id_pat(Pat), id_expr(Expr)};
id_qual({m_generate, Anno, Pat, Expr}) ->
  % Map generator.
  {m_generate, Anno, id_pat(Pat), id_expr(Expr)}.


%% @private Record field expression sequence.
id_field_expr_seq(ExprSeq) ->
  [id_field_expr(Expr) || Expr <- ExprSeq].

%% @private Record field expressions.
id_field_expr({record_field, Anno, Field, Expr}) when
  Field =:= '_'; is_atom(Field) ->
  Field0 = if Field =:= '_' -> Field; true -> id_lit(Field) end,
  {record_field, Anno, Field0, id_expr(Expr)}.


%% @private Bitstring element type specifier list.
id_tsl(TSL) ->
  [id_ts(TS) || TS <- TSL].

%% @private Bitstring element type specifiers.
id_ts(Name) when is_atom(Name) ->
  Name;
id_ts({Name, Value}) when is_atom(Name), is_integer(Value) ->
  {Name, Value}.

%% @private Map associations.
id_assoc_seq(AssocSeq) ->
  [id_assoc(Assoc) || Assoc <- AssocSeq].

%% @private Map associations.
id_assoc({map_field_assoc, Anno, Key, Value}) ->
  % New association.
  {map_field_assoc, Anno, id_expr(Key), id_expr(Value)};

id_assoc({map_field_exact, Anno, Key, Value}) ->
  % Pattern match association.
  {map_field_exact, Anno, id_expr(Key), id_expr(Value)}.


%%% ----------------------------------------------------------------------------
%%% Clauses.
%%% ----------------------------------------------------------------------------

%% @private Clauses.
id_clauses(Clauses) ->
  [id_clause(Clause) || Clause <- Clauses].

%% @private Function, if clauses, case, and catch clauses.

id_clause({clause, Anno, [Pat], [], Body}) when is_list(Body) ->
  % Case clause.
  {clause, Anno, [id_pat(Pat)], [], id_expr_seq(Body)};

id_clause({clause, Anno, [Pat], GuardSeq, Body})
  when
  is_list(GuardSeq), is_list(Body) ->
  % Case clause with guard sequence.
  {clause, Anno, [id_pat(Pat)], id_guard_seq(GuardSeq), id_expr_seq(Body)};

id_clause({clause, Anno, [{throw, Pat, Var}], [], Body}) when is_list(Body) ->
  % Catch clause with pattern.
  {clause, Anno, [{throw, id_pat(Pat), id_pat(Var)}], [], id_expr_seq(Body)};

id_clause({clause, Anno, [{Pat0, Pat1, Var}], [], Body}) when is_list(Body) ->
  % Catch clause with pattern and variables.
  {clause, Anno,
    [{id_pat(Pat0), id_pat(Pat1), id_pat(Var)}], [], id_expr_seq(Body)
  };

id_clause({clause, Anno, [{Pat0, Pat1, Var}], [], Body}) when is_list(Body) ->
  % Catch clause with pattern and variables.
  {clause, Anno,
    [{id_pat(Pat0), id_pat(Pat1), id_pat(Var)}], [], id_expr_seq(Body)
  };

id_clause({clause, Anno, [{throw, Pat, Var}], GuardSeq, Body})
  when
  is_list(GuardSeq), is_list(Body) ->
  % Catch clause with guard sequence and pattern.
  {clause, Anno,
    [{throw, id_pat(Pat), id_pat(Var)}],
    id_guard_seq(GuardSeq), id_expr_seq(Body)
  };

id_clause({clause, Anno, [{Pat0, Pat1, Var}], GuardSeq, Body})
  when
  is_list(GuardSeq), is_list(Body) ->
  % Catch clause with guard sequence, pattern, and variables.
  {clause, Anno,
    [{id_pat(Pat0), id_pat(Pat1), id_pat(Var)}],
    id_guard_seq(GuardSeq), id_expr_seq(Body)
  };

id_clause({clause, Anno, [{Pat0, Pat1, Var}], GuardSeq, Body}) ->
  % Catch clause with guard sequence, pattern, and variables.
  {clause, Anno,
    [{id_pat(Pat0), id_pat(Pat1), id_pat(Var)}],
    id_guard_seq(GuardSeq), id_expr_seq(Body)
  };

id_clause({clause, Anno, PatSeq, [], Body})
  when
  is_list(PatSeq), is_list(Body) ->
  % Function clause.
  {clause, Anno, id_pat_seq(PatSeq), [], id_expr_seq(Body)};

id_clause({clause, Anno, PatSeq, GuardSeq, Body})
  when
  is_list(PatSeq), is_list(GuardSeq), is_list(Body) ->
  % Function clause with guard sequence.
  {clause, Anno, id_pat_seq(PatSeq), id_guard_seq(GuardSeq), id_expr_seq(Body)};

id_clause({clause, Anno, [], GuardSeq, Body})
  when
  is_list(GuardSeq), is_list(Body) ->
  % If clause.
  {clause, Anno, [], id_guard_seq(GuardSeq), id_expr_seq(Body)}.


%%% ----------------------------------------------------------------------------
%%% Guards.
%%% ----------------------------------------------------------------------------

%% @private Guard sequence.
id_guard_seq(GuardSeq) when is_list(GuardSeq) ->
  [id_guard(Guard) || Guard <- GuardSeq].

%% @private Guard.
id_guard(Guard) when is_list(Guard) ->
  [id_guard_test(GuardTest) || GuardTest <- Guard].

%% @private Guard tests.
id_guard_test(Test)
  when
  element(1, Test) =:= atom;
  element(1, Test) =:= char;
  element(1, Test) =:= float;
  element(1, Test) =:= integer;
  element(1, Test) =:= string ->
  % Literal guard test.
  id_lit(Test);
id_guard_test({bin, Anno, GuardTestSeq}) when is_list(GuardTestSeq) ->
  % Bitstring constructor guard test.
  {bin, Anno, id_bin_elem_guard_test_seq(GuardTestSeq)};

id_guard_test({cons, Anno, GuardTestH, GuardTestT}) ->
  % Cons skeleton guard test.
  {cons, Anno, id_guard_test(GuardTestH), id_guard_test(GuardTestT)};

id_guard_test({call, Anno, Name, Guard}) when is_list(Guard) ->
  % Internal function call guard test.
  {call, Anno, id_lit(Name), id_guard(Guard)};

id_guard_test({call, Anno0, {remote, Anno0, Mod, Op}, Guard})
  when
  is_list(Guard) ->
  % External function call guard test.
  Op0 = if is_atom(Op) -> Op; true -> id_lit(Op) end,
  {call, Anno0, {remote, Anno0, id_lit(Mod), Op0}, id_guard(Guard)};

id_guard_test({map, Anno, AssocSeq}) when is_list(AssocSeq) ->
  % Map creation guard test.
  {map, Anno, id_assoc_seq(AssocSeq)};

id_guard_test({map, Anno, GuardTest, AssocSeq}) when is_list(AssocSeq) ->
  % Map update guard test.
  {map, Anno, id_guard_test(GuardTest), id_assoc_seq(AssocSeq)};

id_guard_test({nil, Anno}) ->
  % Nil guard test.
  {nil, Anno};

id_guard_test({op, Anno, Op, GuardTest0, GuardTest1}) ->
  % Binary operator guard test.
  {op, Anno, Op, id_guard_test(GuardTest0), id_guard_test(GuardTest1)};

id_guard_test({op, Anno, Op, GuardTest}) ->
  % Unary operator guard test.
  {op, Anno, Op, id_guard_test(GuardTest)};

id_guard_test({record, Anno, Name, GuardTestSeq}) when is_list(GuardTestSeq) ->
  % Record creation guard test.
  {record, Anno, Name, id_field_guard_test_seq(GuardTestSeq)};

id_guard_test({record_field, Anno, GuardTest, Name, Field}) when is_atom(Field) ->
  % Record field access guard test.
  {record_field, Anno, id_guard_test(GuardTest), Name, Field};

id_guard_test({record_index, Anno, Name, Field}) when is_atom(Field) ->
  % Record field index guard test.
  {record_index, Anno, Name, Field};

id_guard_test({tuple, Anno, Guard}) when is_list(Guard) ->
  % Tuple skeleton guard test.
  {tuple, Anno, id_guard(Guard)};

id_guard_test({var, Anno, Name}) ->
  % Variable guard test.
  {var, Anno, Name}.

%% @private Binary element guard test sequence.
id_bin_elem_guard_test_seq(GuardTestSeq) ->
  [id_bin_elem_guard_test(GuardTest) || GuardTest <- GuardTestSeq].

%% @private Binary element patterns.
id_bin_elem_guard_test({bin_element, Anno, GuardTest, Size, TSL}) ->
  {bin_element, Anno, id_guard_test(GuardTest), id_guard_test(Size), id_tsl(TSL)}.


%% @private Record field expression sequence.
id_field_guard_test_seq(GuardTestSeq) ->
  [id_field_guard_test(GuardTest) || GuardTest <- GuardTestSeq].

%% @private Record field expressions.
id_field_guard_test({record_field, Anno, Field, Expr}) when
  Field =:= '_'; is_atom(Field) ->
  Field0 = if Field =:= '_' -> Field; true -> id_lit(Field) end,
  {record_field, Anno, Field0, id_expr(Expr)}.


%%% ----------------------------------------------------------------------------
%%% Types.
%%% ----------------------------------------------------------------------------

id_type_seq(TypeSeq) when is_list(TypeSeq) ->
  [id_type(Type) || Type <- TypeSeq].

id_type({ann_type, Anno, [Var, Type]}) ->
  % Annotated type.
  {ann_type, Anno, [id_type(Var), id_type(Type)]};
id_type(Type)
  when
  element(1, Type) =:= atom;
  element(1, Type) =:= char;
  element(1, Type) =:= float;
  element(1, Type) =:= integer;
  element(1, Type) =:= string ->
  % Literal type.
  id_lit(Type);
id_type({type, Anno, binary, [Type0, Type1]}) ->
  % Bitstring type.
  {type, Anno, binary, [id_type(Type0), id_type(Type1)]};
id_type({type, Anno, nil, []}) ->
  % Empty list type.
  {type, Anno, nil, []};
id_type({type, Anno, 'fun', []}) ->
  % Generic fun type.
  {type, Anno, 'fun', []};
id_type({type, Anno0, 'fun', [{type, Anno1, any}, Type]}) ->
  % Fun type with parameters.
  {type, Anno0, 'fun', [{type, Anno1, any}, id_type(Type)]};

id_type(Type = {type, _, bounded_fun, _}) ->
  % Constrained fun type.
  id_fun_type(Type);

id_type(Type = {type, _, 'fun', [{type, _, product, _}, _]}) ->
  % Fun type.
  id_fun_type(Type);

id_type({type, Anno, range, [Low, High]}) ->
  % Integer range type.
  {type, Anno, range, [id_type(Low), id_type(High)]};

id_type({type, Anno, map, any}) ->
  % Generic map type.
  {type, Anno, map, any};

id_type({type, Anno, map, AssocTypeSeq}) when is_list(AssocTypeSeq) ->
  % Map type with associations.
  {type, Anno, map, id_assoc_type_seq(AssocTypeSeq)};

id_type({op, Anno, Op, Type0, Type1}) ->
  % Binary operator type.
  {op, Anno, Op, id_type(Type0), id_type(Type1)};

id_type({op, Anno, Op, Type}) ->
  % Unary operator type.
  {op, Anno, Op, id_type(Type)};

id_type({type, Anno, Name, TypeSeq}) when is_list(TypeSeq) ->
  % Built-in type.
  {type, Anno, Name, id_type_seq(TypeSeq)};

id_type({type, ANNO, record, [Name | FieldTypes]}) when is_list(FieldTypes) ->
  % Record type with fields.
  {type, ANNO, record, [id_type(Name) | id_field_type_seq(FieldTypes)]};

id_type({remote_type, Anno, [Mod, Name, TypeSeq]}) when is_list(TypeSeq) ->
  % External type.
  {remote_type, Anno, [id_type(Mod), id_type(Name), id_type_seq(TypeSeq)]};

id_type({type, Anno, tuple, any}) ->
  % Generic tuple type.
  {type, Anno, tuple, any};

id_type({type, ANNO, tuple, TypeSeq}) when is_list(TypeSeq) ->
  % Tuple type.
  {type, ANNO, tuple, id_type_seq(TypeSeq)};

id_type({type, Anno0, union, TypeSeq}) when is_list(TypeSeq) ->
  % Type union.
  {type, Anno0, union, id_type_seq(TypeSeq)};

id_type({var, Anno, Name}) when Name =/= '_'->
  {var, Anno, Name};
  % Type variable.

id_type({user_type, Anno, Name, TypeSeq}) when is_list(TypeSeq)->
  % User-defined type.
  {user_type, Anno, Name, id_type_seq(TypeSeq)}.

%% @private Function types.
id_fun_type({type, Anno, bounded_fun, [FunType, Constr]})
  when
  is_list(Constr) ->
  % Constrained fun type.
  {type, Anno, bounded_fun, [id_fun_type(FunType), id_constr_seq(Constr)]};
id_fun_type({type, Anno0, 'fun', [{type, Anno1, product, TypeSeq}, Type]})
  when
  is_list(TypeSeq) ->
  % Fun type.
  {type, Anno0,
    'fun', [{type, Anno1, product, id_type_seq(TypeSeq)}, id_type(Type)]
  }.

%% @private Function constraint type.
id_constr_seq(ConstrSeq) when is_list(ConstrSeq) ->
  [id_constr(Constr) || Constr <- ConstrSeq].

%% @private Constraint type.
id_constr({type, Anno0, constraint, [{atom, Anno1, is_subtype}, [Var, Type]]}) ->
  {type, Anno0, constraint, [{atom, Anno1, is_subtype}, [id_type(Var), id_type(Type)]]}.


%% @private Association type sequence.
id_assoc_type_seq(AssocTypeSeq) when is_list(AssocTypeSeq) ->
  [id_assoc_type(AssocType) || AssocType <- AssocTypeSeq].

%% @private Association types.
id_assoc_type({type, ANNO, map_field_assoc, [Key, Val]}) ->
  {type, ANNO, map_field_assoc, [id_type(Key), id_type(Val)]};
id_assoc_type({type, ANNO, map_field_exact, [Key, Val]}) ->
  {type, ANNO, map_field_exact, [id_type(Key), id_type(Val)]}.

%% @private Record field type sequence.
id_field_type_seq(TypeSeq) when is_list(TypeSeq) ->
  [id_field_type(Type) || Type <- TypeSeq].

%% @private Record field types.
id_field_type({type, Anno, field_type, [Name, Type]}) ->
  {type, Anno, field_type, [id_type(Name), id_type(Type)]}.
