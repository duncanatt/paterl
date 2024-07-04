%% Checks whether the term is a type.
-define(IS_TYPE(Type), element(1, Type) =:= 'type').

%% Checks whether the term is a literal type.
-define(IS_LIT_TYPE(Type), (?IS_TYPE(Type)
  andalso (element(3, Type) =:= 'boolean'
    orelse element(3, Type) =:= 'integer'
    orelse element(3, Type) =:= 'float'
    orelse element(3, Type) =:= 'string'
    orelse element(3, Type) =:= 'atom'
    orelse element(3, Type) =:= 'unit')
)).

%% Checks whether the term is a mailbox type without modality.
-define(IS_MB_TYPE(Type), (?IS_TYPE(Type)
  andalso not(?IS_LIT_TYPE(Type))
  andalso is_atom(element(3, Type))
)).

%% Checks whether the term is a mailbox type with modality.
-define(IS_MB_MOD_TYPE(Type), (?IS_MB_TYPE(Type)
  andalso (element(4, Type) =:= 'read'
    orelse element(4, Type) =:= 'write')
)).

%% Checks whether the term is a product type.
-define(IS_PROD_TYPE(Type), ?IS_TYPE(Type) andalso element(3, Type) =:= 'prod').

%% Checks whether the term is a union type.
-define(IS_UNION_TYPE(Type), ?IS_TYPE(Type) andalso element(3, Type) =:= 'union').

%% Checks whether the term is a message type.
-define(IS_MSG_TYPE(Type), ?IS_TYPE(Type) andalso element(3, Type) =:= 'msg').


%% Checks whether the term is a variable.
-define(IS_VAR(Term), element(1, Term) =:= 'var').

%% Checks whether the term is a literal.
-define(IS_LIT(Term),
  element(1, Term) =:= 'boolean' orelse
    element(1, Term) =:= 'integer' orelse
    element(1, Term) =:= 'float' orelse
    element(1, Term) =:= 'string' orelse
    element(1, Term) =:= 'atom'
).

%% Checks whether the term is a value.
-define(IS_VAL(Term), (?IS_VAR(Term)
  orelse ?IS_LIT(Term)
  orelse element(1, Term) =:= 'tuple'
  orelse element(1, Term) =:= 'unit'
)).

%% Checks whether the term is an expression.
-define(IS_EXPR(Term), (?IS_VAL(Term)
  orelse element(1, Term) =:= 'msg'
  orelse element(1, Term) =:= 'op'
  orelse element(1, Term) =:= 'call'
  orelse element(1, Term) =:= 'if'
  orelse element(1, Term) =:= 'let'
  orelse element(1, Term) =:= 'new'
  orelse element(1, Term) =:= 'free'
  orelse element(1, Term) =:= 'spawn'
  orelse element(1, Term) =:= 'guard'
  orelse element(1, Term) =:= 'empty'
  orelse element(1, Term) =:= 'receive'
  orelse element(1, Term) =:= 'comment'
)).

%% Checks whether the term is an expression.
-define(IS_OP(Op), (
    Op =:= '+' orelse
      Op =:= '-' orelse
      Op =:= '*' orelse
      Op =:= '/' orelse
      Op =:= '==' orelse
      Op =:= '<' orelse
      Op =:= '>' orelse
      Op =:= '>=' orelse
      Op =:= '<=' orelse
      Op =:= '!'
)).

%% Checks whether the term is a pattern.
-define(IS_PAT(Term), element(1, Term) =:= 'pat').

%% Checks whether the term is a message pattern.
-define(IS_MSG_PAT(Term), ?IS_PAT(Term) andalso element(3, Term) =:= 'msg').


