# paterl - Erlang to Pat transpiler

The current type-checking pipeline consists of these stages:

1. `epp:parse_file`: Erlang preprocessor (parsing and macro expansion)
2. `paterl_syntax:module`: Rule out unsupported Erlang syntactic subset (not implemented)
3. `erl_lint:module`: Erlang linting
4. `paterl_ir:module`: Assignment transformation which expands Erlang expressions to match (will implement ANF next)
5. `paterl_types:table`: Extract Erlang `typespec` annotations into a table
6. `paterl_bootstrap:forms`: Modifies the Erlang syntax tree to insert an auxiliary bootstrapping `main` function
7. `paterl_anno:annotate`: Annotates the Erlang syntax tree using the type information in the table obtained from step 5
8. `paterl_trans:module`: Translates the annotated Erlang syntax tree to a Pat syntax tree
9. `pat_prettypr:module`: Prints the Pat syntax tree

## Using from the Erlang shell

```erlang
paterl:compile("src/examples/erlang/codebeam/id_server_demo.erl", [{includes, ["include"]}, {out, "out"}]).
```

## Using from command line

```shell
./src/paterl src/examples/erlang/codebeam/id_server_demo.erl -v all -I include
```

