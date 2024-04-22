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
interface Main { }

# -spec future_fun() -> no_return().
# future_fun() ->
#   ?mb_state("put.get*"),
#   receive
#     {put, X} ->
#       resolved_future(X)
#   end.
def future_fun(mb0: Future?): (Unit * Future?) {
  guard mb0: Put.*Get {
    receive Put(x) from mb1 ->
      resolved_future(mb1, x)
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
def resolved_future(mb2: Future?, x:Int): (Unit * Future?) {
  guard mb2: *Get {
    empty(mb2) -> ((), mb2)
    receive Get(user) from mb3 ->
      let (t0, mb4) =
        (user ! Reply(x), mb3)
      in
        t0;
        resolved_future(mb4, x)
  }
}

# -spec user(pid()) -> integer().
# user(Future) ->
#   Self = self()
#   Future ! {get, Self},
#   ?mb_state("reply"),
#   receive
#     {reply, X} ->
#       ?free,
#       X
#   end.
def user(mb5: User?, future: Future!): (Int * User?) {
  let (self, mb6) =
    (mb5, mb5)
  in
    let (t1, mb7) =
      (future ! Get(self), mb6)
    in
      t1;
      guard mb7: Reply {
        receive Reply(x) from mb8 ->
          (x, mb8) # Condition that if the expression list is one element long, this would be a shortcut instead of using lets.
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
def main(mb9: Main?): (Unit * Main?) {

  let (future_mb, mb10) =
      (let mb11 =
        new [Future]
      in
        let t2 =
          spawn { let (t3, mb12) = future_fun(mb11) in t3; free(mb12) }
        in
          t2;
          (mb11, mb9))
  in
    let (t4, mb12) =
      (future_mb ! Put(5), mb10)
    in
      t4;

      let (a, mb13) =
        (let mb14 =
          new [User]
        in
          let (x, mb15) =
            user(mb14, future_mb)
          in
            let y =
              free(mb15)
            in
              (x, mb12))
      in
        (print(intToString(a)), mb13)
}

def main0(): Unit {
  let main_x =
    new [Main]
  in
    let (tX, main_y) =
      main(main_x)
    in
      let tY =
        free(main_y)
      in
        tX
}

