#### Two receives in sequence (with state-passing translation and using many
#### aliases to self).
#### File: two_receives_seq__many_aliases_test.typ
#### Result: Client-side translation succeeds with translation 3???

### Interface definitions.

interface ServerMb { Add(ClientMb!, Int, Int), Mul(ClientMb!, Int, Int) }
interface ClientMb { Ans(Int) }

## Typed Erlang (server side).

def server(mb0: ServerMb?): Unit {
guard mb0: *Add.*Mul {
  receive Add(client, a, b) from mb1 ->
    let (x0, mb2) =
      (client ! Ans(a + b), mb1)
    in
      x0;
      server(mb2)
    receive Mul(client, a, b) from mb1 ->
      let (x0, mb2) =
        (client ! Ans(a * b), mb1)
      in
        x0;
        server(mb2)
    free -> ()
  }
}

## Typed Erlang (client side).

def main0(): Int {
  let (x0, mb0) =
    (let mb1 =
       new [ClientMb]
     in
       main(mb1))
  in
    let x1 =
      free(mb0)
    in
      x1;
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
        (mb3, mb3)
      in
        let (self1, mb5) =
          (mb4, mb4)
        in
          let (x1, mb6): (Unit * ClientMb?) = # One way of ensuring that type synthesis occurs.
            (serverPid1 ! Add(self0, 1, 2), mb5)
          in
            let (x2, mb7) = # This is the other way, not annotating let..
              (serverPid0 ! Mul(self1, 3, 4), mb6)
            in
              x2; # .. but then using the unit variable to enable type synthesis!
              let (a, mb9) =
                guard mb7: Ans.Ans {
                  receive Ans(a) from mb8 ->
                    (a, mb8)
                }
              in
                let (b, mb11) =
                  guard mb9: Ans {
                    receive Ans(b) from mb10 ->
                      (b, mb10)
                  }
                in
                  (a + b, mb11)
}