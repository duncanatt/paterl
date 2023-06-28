%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2023 11:25
%%%-------------------------------------------------------------------
-module(future_test).
-author("duncan").


%%-export([main/0]).
-compile(export_all).

%% Associate mailbox type to functions that adhere to the mailbox type.
%% These define the mailbox type with the functions that manipulate that mailbox.
-future([future_fun/0, full_future/1]). % Error message would be: Mailbox type 'future' is not defined.
-user([user/0]).
%%-duncan([ff/0]).


-type singly() :: {singly, pid()}.
-type put() :: {putty, integer()}.
-type get() :: {get, user()}. % may also be a PID type?
-type reply() :: {reply, integer()}.
-type untagged_tuple() :: {55, atom, integer(), none()}.
-type empty_tuple() :: {}.

%%-type fun_test() :: fun((...) -> integer()).

%%-type future() :: put() | get().
%%-type user() :: reply().

-type future() :: pid() | put() | get().
-type user() :: pid() | reply().
-type dummy() :: pid().

-type erroneous(T) :: T.
-type erroneous2(T) :: T.



% If the type is a pid, we know that there is a type union of messages.



% Should be imported from an include file.
-define(state(Regex), "@regex:" ++ Regex).


%% TODO: Ultimate aim.
% Another way of extracting would be:
% 1. get the attributes and the functions.
% 2. from these functions get the list of regexes.
% 3. parse these regexes and extract the list of messages.
% 4. associate the list of messages (which would be a union of types) to the mailbox type.
% The rationale is to construct a least interface type (or union). This would make sense since
% in Pat interfaces, we can declare more message types and we can use less in the regex.
% So the messages in the regex are a subset inclusion.


-spec future_fun() -> no_return().
future_fun() ->
  5.6,
  {},
  {hello},
  {there, 5},
  {5},
  {fun() -> ok end, duncan},
  {10 + 2, ok},

  K = 197,
  X = 10,
  200 = X,
  {tag, X} = 200,
  if
    K + 2 =:= {} -> ok;
    K -> adrian;
    true -> not_ok
  end,

  Y = 22 + 11,
  2 + 1 = Y,
  +1 = 2,
  2 = +1,


  ?state("put()"), % The type here must be defined above.
  receive
    {put, X} ->
      full_future(X), io:format(""), (fun() -> io end()):(fun() -> format end())("Printing from function~n");
    {DummyVar1, atom2} ->
      (fun() -> full_future end())(10);
    {} ->
      ok;
    D ->
      ok;
    {5.5, ok} ->
      duncan;
    matt ->
      ok;
    77.7 ->
      jas
  end.

-spec full_future(pid()) -> no_return().
full_future(X) ->
  "@regex: put().get()*", %% When we find star, we immediately enter the free clause as one of the guards of the receive.
  ?state("put().get()*"),
  receive
    {get, User} ->
      User ! {reply, X},
      full_future(X);
    {put, _} ->
      ok
  end.


-spec user(future(), atom()) -> integer() ; (future(), atom()) -> float().
%%-spec user(future(), atom()) -> integer().
%%-spec user(future(), cikku()) -> peppi().
%%-spec user(future(), cikku()) -> peppi().
user(FuturePid, true) ->
  %% New user mailbox is created automatically. An implicit reference is obtained via self.

  FuturePid ! {get, self()},
  ?state("reply()"),
  receive
    {reply, X, vik} ->
      X % Free is implied by the end of the regex containing reply() which becomes an empty string.
      % So empty string or * always necessitate synthesising a free in Pat.
  end.


main() ->
  Pid = spawn(fun() -> future_fun() end), %% This might have to achieved using MFArgs ?MODULE, fun, Args.
  Pid ! {put, 5},
  user(Pid, true).
