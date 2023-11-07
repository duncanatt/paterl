#### Two receives in sequence (with state-passing translation and using many
#### aliases to self).
#### File: two_receives_seq__many_aliases_test.typ
#### Result: Client-side translation succeeds with translation 3???

### Interface definitions.

interface ServerMb { Add(ClientMb!, Int, Int), Mul(ClientMb!, Int, Int) }
interface ClientMb { Ans(Int) }

## Typed Erlang (server side).

def server(mb0: ServerMb?): Unit {
#def server(mb0: ServerMb?): (Unit * ServerMb?) {
  guard mb0: *Add . *Mul {
    receive Add(client, a, b) from mb1 ->
      let (x0, mb2): (Unit * ServerMb?) =
        (client ! Ans(a + b), mb1)
      in
        server(mb2)
    receive Mul(client, a, b) from mb1 ->
      let (x0, mb2): (Unit * ServerMb?) =
        (client ! Ans(a * b), mb1)
      in
        server(mb2) # See whether I can correct the function invocation to use pairs!!
    free -> ()
  }
}

## Typed Erlang (client side).

def main0(): Int {
  let (x0, mb1) =
    (let mb1 =
       new [ClientMb]
     in
       main(mb1))
  in
    let x1: Unit =
      free(mb1)
    in
      x0
}

def main(mb0: ClientMb?): (Int * ClientMb?) {
  let (serverPid0, mb1) =
    (let mb2 =
      new [ServerMb]
    in
      let x0 =
        spawn { server(mb2) }
      in
        (mb2, mb0))
  in
    let (serverPid1, mb3) =
      (serverPid0, mb1)
    in
      let (self0, mb4) =
        get_self(mb3)
        #(mb3, mb3)
      in
        let (self1, mb5) =
          #get_self(mb4) # Does not work when I enable this. It says assertion failed. Why?
          (mb4, mb4)
        in
          let (x1, mb6) =
            (serverPid1 ! Add(self0, 1, 2), mb5)
          in
            x1;
            let (x2, mb7) =
              (serverPid0 ! Mul(self1, 3, 4), mb6)
            in
              x2;
              let (a, mb9) =
                guard mb7: Ans.Ans {
                  receive Ans(a) from mb10 ->
                    (a, mb10)
                }
              in
                let (b, mb11) =
                  guard mb9: Ans {
                    receive Ans(b) from mb10 ->
                      (b, mb10)
                  }
                in
                  (add(a, b), mb11)
}

def get_self(mb0: ClientMb?): (ClientMb! * ClientMb?) {
  (mb0, mb0)
}

#def get_self(mb0: ServerMb?): (ServerMb! * ServerMb?) {
#  (mb0, mb0)
#}

def add(a: Int, b: Int): Int {
  a + b
}