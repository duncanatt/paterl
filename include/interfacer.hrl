%%%-------------------------------------------------------------------
%%% Encoding of Interfacer's proposed notation in unmodified Erlang.
%%%
%%%   proposed                              encoded here
%%%   ------------------------------------  ----------------------------------
%%%   pid(I)                                pid_of(I)
%%%   -interface f :: I().                  {f, I} in one -interface([...])
%%%   -interface I().           (module)    -interface(I).
%%%
%%% Three things the encoding cannot preserve.
%%%
%%% The type parameter is phantom: pid_of(a()) and pid_of(b()) both erase to
%%% pid(), so external analysers neither enforce the interface nor its variance.
%%% The encoding buys compilation and the ordinary type checking, not the interface 
%%% checking.
%%%
%%% Placement. Erlang requires user-defined attributes to precede every function
%%% definition, and -spec is exempt from that rule. An encoded per-function
%%% declaration therefore cannot stand next to its function, and the entries are
%%% collected in a single -interface([...]) at the top of the module. Were
%%% -interface added to the language it should be granted -spec's exemption, so
%%% that a declaration can sit with the function it describes.
%%%
%%% Type references become atoms. In -interface f :: I(). the interface is a
%%% type reference; in the encoded {f, I} it is a bare atom, which erlc does not
%%% count as a use, so an interface named nowhere else is reported unused. The
%%% examples export those types to silence it. The real declaration would need
%%% no such workaround.
%%%-------------------------------------------------------------------

-type pid_of(_I) :: pid().
