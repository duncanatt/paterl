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
      let (x0, mb1) =
        (client ! Ans(a + b), mb1)
      in
        x0;
        server(mb1)
    receive Mul(client, a, b) from mb1 ->
      let (x0, mb1) =
        (client ! Ans(a * b), mb1)
      in
        x0;
        server(mb1)
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
  let x1: Unit =
    free(mb0)
  in
    #x1;
    x0
}

def main(mb0: ClientMb?): (Int * ClientMb?) {
  let (serverPid, x0) =
    (let mb1 =
      new [ServerMb]
    in
      let x1 =
        spawn { server(mb1) }
      in
        (mb1, mb0))
  in
    #x0; # It is unit for the moment, but we'll change it to become mb too.
    let (self0, mb2)  =
      (x0, x0)
    in
      let (self1, mb3)  =
        (mb2, mb2)
      in
        let (x2, mb4) =
          (serverPid ! Add(self0, 1, 2), mb3)
        in
          x2;
          let (x3, mb5) =
            (serverPid ! Add(self1, 3, 4), mb4)
          in
            x3;
            let (a, mb6) =
              guard mb5: Ans.Ans {
                receive Ans(a) from mb7 ->
                  (a, mb7)
              }
            in
              let (b, mb8) =
                guard mb6: Ans {
                  receive Ans(b) from mb9 ->
                    (b, mb9)
                }
            in
              (a + b, mb8)
}
