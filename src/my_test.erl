%%%-------------------------------------------------------------------
%%% @author duncan
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2023 14:41
%%%-------------------------------------------------------------------
-module(my_test).
%%-author("duncan").

%% API
%%-export([]).

%% Associate mailbox type to functions that adhere to the mailbox type.
%% These define the mailbox type with the functions that manipulate that mailbox.
%%-future([future_fun/0, full_future/1]). % Error message would be: Mailbox type 'future' is not defined.
%%-user([user/0]).


%%-type put() :: {putty, integer()}.
%%-type get() :: {get, user()}. % may also be a PID type?
%%-type reply() :: {reply, integer()}.

%%-type fun_test() :: fun((...) -> integer()).

%%-type future() :: put() | get().
%%-type user() :: reply().

%%-type future() :: pid() | put() | get().
%%-type user() :: pid() | reply().



% If the type is a pid, we know that there is a type union of messages.



% Should be imported from an include file.
%%-define(state(Regex), "@regex:" ++ Regex).

-define(st, ok).
-type put() :: {putty, integer(), ?st}.