#### Two receives in sequence (with state-passing translation and using one
#### alias to self).
#### File: two_receives_seq__one_alias_to_self.typ
#### Result: Client-side translation FAILS with translation 0.

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
    let (x0, self) =
      ((), mb0)
    in
      x0; # Use unit value.
      let (x1, mb3) =
        (serverPid ! Add(self, 1, 2), self)
      in
        x1; # Use unit value.
        let (x2, mb4) =
          # (serverPid ! Mul(self, 3, 4), mb3) # [CONSTRAINT GENERATION (Join)] Mailbox variable self was used after being consumed by a 'guard' or a 'let' binding.
          (serverPid ! Mul(mb3, 3, 4), mb3) # Works.
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

