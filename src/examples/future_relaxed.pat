# Mailbox interfaces.
interface Future_mb { Put(Int), Get(User_mb!) }
interface User_mb { Reply(Int) }
interface Main_mb { }

def future(mb0: Future_mb?): (Unit * Future_mb?) {
  guard mb0: Put.*Get {
    receive Put(x) from mb1 ->
      resolved_future(mb1, x)
  }
}

def resolved_future(mb0: Future_mb?, x: Int): (Unit * Future_mb?) {
  guard mb0: *Get {
    empty(mb1) -> ((), mb1)
    receive Get(user) from mb1 ->
      let (t0, mb2) =
        (user ! Reply(x), mb1)
      in
        t0;
        resolved_future(mb2, x)
  }
}

def user(mb0: User_mb?, future: Future_mb!): (Int * User_mb?) {
  let (self, mb1) =
    (mb0, mb0)
  in
    let (t0, mb2) =
      (future ! Get(self), mb1)
    in
      t0;
      guard mb2: Reply {
        receive Reply(x) from mb3 ->
          (x, mb3)
          #((), mb3)
      }
}

def main(mb0: Main_mb?): (Unit * Main_mb?) {
  let (future_mb, mb3) =
      (let mb1 =
        new [Future_mb]
      in
        let y =
          spawn { let (x, mb2) = future(mb1) in free(mb2); x }
        in
          mb1, mb0)
  in
    let (t0, mb4) =
      (future_mb ! Put(5), mb3)
    in
      t0;

      let (a, mb5) =
        (let mb6 =
          new [User_mb]
        in
          let (x, mb7) =
            user(mb6, future_mb)
          in
            let y =
              free(mb7)
            in
              x, mb4)
      in
        (print(intToString(a)), mb5)
}

# Might have to be automagically inserted.
def main0(): Unit {
  let mb0 =
    new [Main_mb]
  in
    let (x, mb1) =
      main(mb0)
    in
      let y =
        free(mb1)
      in
        x
}

def no_mb_fun(): Int {
  let (x, t0) =
    (5, ())
  in
    t0;
    let (y, t1) =
      (10, ())
    in
      t1;
      x + y
}

def let_problem(x: String): (Int * String) {
  (let y = 10 in y, x) # Works.
  #(let x = 10 in x, x) # Fails.
}

def let_test(): Unit {
  let x =
    spawn { () } # Works.
    #spawn { 1 } # Fails. Is the expression in spawn expected to return ()?
    #spawn { (1, 1) } # Fails for the same reason.
  in
    #x
    ()
}

def linearity_test(): Unit {
  let x = 5 in ()
}