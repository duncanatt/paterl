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

interface Future_mb { Put(Int), Get(User_mb!) }
interface User_mb { Reply(Int) }
interface Main_mb { }

# -spec future_fun() -> no_return().
# future_fun() ->
#   ?mb_state("put.get*"),
#   receive
#     {put, X} ->
#       resolved_future(X)
#   end.
def future(mb0: Future_mb?): (Unit * Future_mb?) {
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
def resolved_future(mb2: Future_mb?, x:Int): (Unit * Future_mb?) {
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
def user(mb5: User_mb?, future: Future_mb!): (Int * User_mb?) {
  let (self, mb6) =
    (mb5, mb5)
  in
    let (t1, mb7) =
      (future ! Get(self), mb6)
    in
      t1;
      guard mb7: Reply {
        receive Reply(x) from mb8 ->
          (x, mb8)
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
def main(mb9: Main_mb?): (Unit * Main_mb?) {

  let (future_mb, mb10) =
      (let mb11 =
        new [Future_mb]
      in
        let t2 =
          spawn { let (t3, mb12) = future(mb11) in t3; free(mb12) }
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
          new [User_mb]
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

# Might have to be automagically inserted.
def main0(): Unit {
  let main_x =
    new [Main_mb]
  in
    let (tX, main_y) =
      main(main_x)
    in
      let tY =
        free(main_y)
      in
        tX
}

