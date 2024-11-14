%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% Error handling and management.
%%% @end
%%% Created : 31. Jan 2024 09:39
%%%-------------------------------------------------------------------
-author("duncan").

%% Error creation macros.
-define(
pushError(Code, Node, Error),
%%  [{erl_syntax:get_pos(ErrNode), ?MODULE, {Class, ErrNode}} | Errors]
%%  Error#error{errors =
%%  [{erl_syntax:get_pos(ErrNode), ?MODULE, {Class, ErrNode}} | Error#error.errors]
%%  }
  paterl_lib:push_error(?MODULE, {Code, Node}, Error)
).

-define(
pushWarning(Code, Node, Error),
  paterl_lib:push_warning(?MODULE, {Code, Node}, Error)
).

%%-define(
%%pushError(Class, Reason, ErrNode, Errors),
%%  [{erl_syntax:get_pos(ErrNode), ?MODULE, {Class, Reason, ErrNode}} | Errors]
%%).

-record(error, {
  errors = [] :: errors:reasons(),
  warnings = [] :: errors:reasons()
}).

%%-record(ok, {
%%  warnings = [] :: errors:errors()
%%}).
%%
%%-record(ok, {
%%  result :: any(),
%%  warnings = [] :: errors:errors()
%%}).