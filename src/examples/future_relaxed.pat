# Mailbox interfaces.
interface Future_mb {
  Put(Int),
  Get(User_mb!)
}
interface User_mb {
  Reply(Int)
}
interface Main_mb { }
def future(mb0: Future_mb?): (Unit * Future_mb?) {
  guard mb0: Put.*Get {
    receive Put(x) from mb1 ->
      resolved_future(mb1, x)
  }
}
def resolved_future(mb0: Future_mb?, x: Int): (Unit * Future_mb?) {
  guard mb0: *Get {
    empty(mb1) ->
      ((), mb1)
    receive Get(user) from mb1 ->
      let (t0, mb2) =
        (user ! Reply(x), mb1)
      in
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
      guard mb2: Reply {
        receive Reply(x) from mb3 ->
          (x, mb3)
      }
}
def main(mb0: Main_mb?): (Unit * Main_mb?) {
  let (future_mb, mb1) =
    (let mb0 =
      new [Future_mb]
    in
      let y =
        spawn {
          let (x, mb1) =
            future(mb0)
          in
            free(mb1)
        }
      in
        mb0, mb0)
  in
    let (t0, mb2) =
      (future_mb ! Put(5), mb1)
    in
      let (a, mb3) =
        (let mb0 =
          new [User_mb]
        in
          let (x, mb1) =
            user(mb0, future_mb)
          in
            let y =
              free(mb1)
            in
              x, mb2)
      in
        let (t1, mb4) =
          (print(intToString(a)), mb3)
        in
          if (1 == 1) {
            (t1, mb4)
          }
          else {
            (t1, mb4)
          }
}
def main'(): Unit {
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
