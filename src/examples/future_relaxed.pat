# %% Mailbox interface-function associations.
# -future([future_fun/0, resolved_future/1]). % Always marks the receiving end.
# -user([user/1]). % Always marks the receiving end.

# %% Message types.
# -type put() :: {put, integer()}.
# -type get() :: {get, user()}.
# -type reply() :: {reply, integer()}.

# %% Mailbox interfaces.
# -type future() :: pid() | put() | get().
# -type user() :: pid() | reply().

interface Future { Put(Int), Get(User!) }
interface User { Reply(Int) }

# -spec future_fun() -> no_return().
# future_fun() ->
#   ?mb_state("put.get*"),
#   receive
#     {put, X} ->
#       resolved_future(X)
#   end.
def future_fun(self: Future?): (Unit * Future?) {
  guard self: Put.*Get {
    receive Put(x) from self->
      resolved_future(self, x)
  }
}

# -spec resolved_future(integer()) -> no_return().
# resolved_future(X) ->
#   ?mb_state("get*"),
#   receive
#     {get, User} -> % of type get().
#       User ! {reply, X},
#       resolved_future(X)
#   end.
def resolved_future(self: Future?, x:Int): (Unit * Future?) {
  guard self: *Get {
    empty(self) -> ((), self)
    receive Get(user) from self ->
      user ! Reply(x);
      resolved_future(self, x)
  }
}

# -spec user(pid()) -> integer().
# user(Future) ->
#   Future ! {get, self()},
#   ?mb_state("reply"),
#   receive
#     {reply, X} ->
#       ?free,
#       X
#   end.
def user(self: User?, future: Future!): (Int * User?) {
  future ! Get(self);
  guard self: Reply {
    receive Reply(x) from self ->
      (x, self)
  }
}

# Annotate?
# main() ->
#   Future = spawn(?MODULE, future_fun, []),
#   Future ! {put, 5},
#
#   TECHNICALLY, THIS IS THE SAME MAIN MAILBOX SHARED BETWEEN TWO USER() FUNCTIONS.
#   format("Got ~p.~n", [user(Future)]),
#   format("Got ~p.~n", [user(Future)]).
def main(): Unit {



  #let future_mb = new [Future] in
  #let y =
  #  spawn { let (x, future_mb1) = future_fun(future_mb) in x; free(future_mb1) }
  #  in future_mb;

  let future_mb =
      (let mb2 =
        new [Future]
      in
        let x0 =
          spawn { let (xx, mb) = future_fun(mb2) in xx; free(mb) }
        in
          mb2)
  in

  future_mb ! Put(5);

  let user_mb = new [User] in

  let (a, user_mb0) = user(user_mb, future_mb) in
  print(intToString(a));

  let (b, user_mb1) = user(user_mb0, future_mb) in
  print(intToString(b));
  free(user_mb1)
}

