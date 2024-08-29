#### Two receives in sequence (with state-passing translation and using self as
#### an argument).
#### File: two_receives_seq__inline_self.typ
#### Result: Client-side translation succeeds with translation 0.

### Interface definitions.

interface ServerMb { Add(ClientMb!, Int, Int), Mul(ClientMb!, Int, Int) }
interface ClientMb { Ans(Int) }

# Server code developed manually.
def server(mb0: ServerMb?): Unit {
  guard mb0: *Add.*Mul {
    receive Add(client, a, b) from mb1 ->
        client ! Ans(a + b);
        server(mb1)
    receive Mul(client, a, b) from mb1 ->
        client ! Ans(a * b);
        server(mb1)
    free -> ()
  }
}

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
  let (serverPid, mb1) =
    (let mb2 =
      new [ServerMb]
    in
      let x0 =
        spawn { server(mb2) }
      in
        (mb2, x0))
  in
    mb1; # Use unit value.
    let (x1, mb3) =
      (serverPid ! Add(mb0, 1, 2), mb0)
    in
      x1; # Use unit value.
      let (x2, mb4) =
        (serverPid ! Mul(mb3, 2, 4), mb3)
      in
        x2; # Use unit value.
        let (a, mb6) =
          guard mb4: Ans.Ans {
            receive Ans(a) from mb5 ->
              (a, mb5)
          }
        in
          let (b, mb8) =
            guard mb6: Ans {
              receive Ans(b) from mb7 ->
                (b, mb7)
            }
          in
            (a + b, mb8)
}