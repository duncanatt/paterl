%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2023 11:25
%%%-------------------------------------------------------------------
-module(future).
-author("duncan").

-export([main/0]).

%% Associate mailbox type to functions that adhere to the mailbox type.
%% These define the mailbox type with the functions that manipulate that mailbox.
-future([future_fun/0, full_future/1]). % Error message would be: Mailbox type 'future' is not defined.
-user([user/0]).


-type put() :: {putty, integer()}.
-type get() :: {get, user()}. % may also be a PID type?
-type reply() :: {reply, integer()}.

%%-type fun_test() :: fun((...) -> integer()).

%%-type future() :: put() | get().
%%-type user() :: reply().

-type future() :: pid() | put() | get().
-type user() :: pid() | reply().



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


-spec future_fun() -> none().
future_fun() ->
  ?state("put()"), % The type here must be defined above.
  receive
    {put, X} ->
      full_future(X)
  end.

-spec full_future(integer()) -> none().
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


-spec user(future()) -> integer().
user(FuturePid) ->
  %% New user mailbox is created automatically. An implicit reference is obtained via self.

  FuturePid ! {get, self()},
  ?state("reply()"),
  receive
    {reply, X} ->
      X % Free is implied by the end of the regex containing reply() which becomes an empty string.
      % So empty string or * always necessitate synthesising a free in Pat.
  end.


main() ->
  Pid = spawn(fun() -> future_fun() end),
  Pid ! {put, 5},
  user(Pid).
