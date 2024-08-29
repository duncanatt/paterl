#### Two nested receives (with state-passing translation and using self as
#### an argument directly).
#### File: two_nested_receives__inline_self.typ
#### Result: Client-side translation succeeds with translation 0.

### Interface definitions.

interface ServerMb { Add(ClientMb!, Int, Int), Mul(ClientMb!, Int, Int) }
interface ClientMb { Ans(Int) }

# Translated code, but added free manually.
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
    free -> () # Added manually.
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
  let (serverPid, x0) =
    (let mb1 =
      new [ServerMb]
    in
      let x1 =
        spawn { server(mb1) }
      in
        (mb1, x1))
  in
    x0;
    let (x2, mb2) =
      (serverPid ! Add(mb0, 1, 2), mb0)
    in
      x2;
      let (x3, mb3) =
        (serverPid ! Mul(mb2, 3, 4), mb2)
      in
        x3;
        guard mb3: Ans.Ans {
          receive Ans(a) from mb4 ->
            guard mb4: Ans {
              receive Ans(b) from mb5 ->
                (a + b, mb5)
            }
        }
}