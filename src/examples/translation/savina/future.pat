interface Future {
  Put(Int),
  Get(User!)
}

interface User {
  Reply(Int)
}

def future(mb0: Future?): (Unit * Future?) {
  guard mb0: Put.*Get {
    receive Put(x) from mb1 ->
      resolved_future(mb1, x)
  }
}

def resolved_future(mb0: Future?, value: Int): (Unit * Future?) {
  guard mb0: *Get {
    receive Get(userPid) from mb1 ->
        let (x0, mb2) =
          (userPid ! Reply(value), mb1)
        in
          x0;
          resolved_future(mb2, value)
    empty(mb1) ->
      ((), mb1)
  }
}

def user(mb0: User?, futurePid:Future!): (Int * User?) {
  let (self, mb1) =
    (mb0, mb0)
  in
    let (x0, mb2) =
      (futurePid ! Get(self), mb1)
    in
      x0;
      guard mb2: Reply {
        receive Reply(x) from mb3 ->
          (x, mb3)
      }
}

def main0(): Unit {
  let (x0, mb0) =
    (let mb1 =
      new [User]
    in
      main(mb1))
  in
    let x1 =
      free(mb0)
    in
    x1;
    x0
}

def main(mb0: User?): (Unit * User?) {
  let (futurePid, mb2) =
    (let mb1 =
      new [Future]
    in
      let x0 =
        spawn { let (x1, mb3) = future(mb1) in free(mb3); x1 }
      in
        #x0;
        (mb1, mb0))
  in
    let (x1, mb4) =
      (futurePid ! Put(5), mb2)
    in
      x1;
      let (get1, mb5) =
        (let mb6 =
          new [User]
        in
          let (x2, mb7) =
            user(mb6, futurePid)
          in
            let x3 =
              free(mb7)
            in
              #x3;
              (x2, mb4))
      in
        (print(intToString(get1)), mb5)
}
