#### Two receives in sequence (with state-passing translation and using one
#### alias to self).
#### File: two_receives_seq__one_alias_to_self.typ
#### Result: Client-side translation fails with translation 0.

### Interface definitions.

interface ServerMb { Add(ClientMb!, Int, Int), Mul(ClientMb!, Int, Int) }
interface ClientMb { Ans(Int) }

def server(mb0: ServerMb?): Unit {
  guard mb0: *Add.*Mul {
    free -> ()
    receive Add(client, a, b) from mb1 ->
        client ! Ans(a + b);
        server(mb1)
    receive Mul(client, a, b) from mb1 ->
        client ! Ans(a * b);
        server(mb1)
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
      #(let (x2, mb4) =
      #  (serverPid, mb0)
      #in
        (serverPid ! Add(mb0, 1, 2), mb0)
    in
      x1; # Use unit value. Solution is to rework the translation clause for send to not evaluate the value for now. Clause should read x ! m, not e ! m.
      let (x3, mb5) =
        #(let (x4, mb6) =
          # (serverPid, mb3) # Fails: [CONSTRAINT GENERATION (Join)] Mailbox variable serverPid was used after being consumed by a 'guard' or a 'let' binding.
          #(x2, mb3) # But this works! We need a way to solve this aliasing problem.
        #in
          (serverPid ! Mul(mb3, 3, 4), mb3)
      in
        x3; # Use unit value.
        let (a, mb7) =
          guard mb5: Ans.Ans {
            receive Ans(a) from mb8 ->
              (a, mb8)
          }
        in
          let (b, mb9) =
            guard mb7: Ans {
              receive Ans(b) from mb10 ->
                (b, mb10)
            }
          in
            (a + b, mb9)
}