interface Future {
 Put(Int),
 Get(User!),
 Dummy()
}

interface User {
  Reply(Int)
}

def future(mb0: Future?): (Unit * Future?) {
  guard mb0: Put.*Get {
    receive Put(value) from mb1 ->
      resolved_future(mb1, value)
  }
}

def resolved_future(mb0: Future?, value: Int): (Unit * Future?) {
  guard mb0: *Get {
    receive Put(value) from mb1 -> #??
      ((), mb1) #??
    receive Dummy() from mb1 -> #??
      ((), mb1) #??
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

def user(mb0: User?, futurePid: Future!): (Int * User?) {
  let (self, mb1) =
    (mb0, mb0)
  in
    let (x0, mb2) =
      (futurePid ! Get(self), mb1)
    in
      x0;
      guard mb2: Reply {
        receive Reply(value) from mb3 ->
          (value, mb3)
      }
}

def main(mb0: User?): (Unit * User?) {
  let (futurePid, mb1) =
    (let mb2 =
        new [Future]
      in
        let x0 =
          spawn { let (x1, mb3) = future(mb2) in free(mb3); x1 }
        in
          mb2
    , mb0)
  in
    let (x2, mb4) =
      (futurePid ! Put(5), mb1)
    in
      x2;
      let (get1, mb7) =
        (let mb5 =
          new [User]
        in
          let (x3, mb6) =
            user(mb5, futurePid)
          in
            let x4 =
              free(mb6)
            in
              x3
        , mb4)
      in
        let (x5, mb8) =
          (print(intToString(get1)), mb7)
        in
          x5;
          let (get2, mb11) =
            (let mb9 =
              new [User]
            in
              let (x6, mb10) =
                user(mb9, futurePid)
              in
                let x7 =
                  free(mb10)
                in
                  x6
            , mb8)
          in
            (print(intToString(get2)), mb11)
}

def main0(): Unit {
  let mb0 =
    new [User]
  in
    let (x0, mb1) =
      main(mb0)
    in
      let x1 =
        free(mb1)
      in
        x0
}


