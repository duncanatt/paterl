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

#def main0():Int {
#  let mb0 =
#    new [ClientMb]
#  in
#    let (x0, mb1) =
#      main(mb0)
#    in
#      free(mb1);
#      x0
#}

#def main(mb0: ClientMb?): (Int * ClientMb?) {

#  let serverPid = new [ServerMb] in
#  spawn { server(serverPid) };

  # Does not work. But see below comment.
#  let (x0, mb1) =
#    (serverPid ! Add(mb0, 1, 2), mb0)
#  in
#    x0;
#    guard mb1: Ans {
#      receive Ans(a) from mb2 ->
#        # x0; # <--- If I uncomment this line (and use up x0), then it works. Is this the correct way to use x0?
#        (a, mb2)
#    }

#}


def main0(): Int {
  let mb0 =
    new [ClientMb]
  in
    let (x0, mb1) =
      main(mb0)
    in
      free(mb1);
      x0
}

#def main(mb2: ClientMb?): (Int * ClientMb?) {
#
#  let (serverPid, mb3) =
#    (let mb4 =
#      new [ServerMb]
#    in
#      spawn { server(mb4) }; # Free is unhandled for now.
#      (mb4, ())) # Nothing to pass along since this is a spawn.
#  in
#    mb3;
#    let (self, mb5) =
#      (mb2, ())
#    in
#      mb5;
#      let (addA, mb6) =
#        (1, self)
#      in
#        let (addB, mb7) =
#          (2, mb6)
#        in
#          let (x1, mb8) =
#            (serverPid ! Add(mb7, addA, addB), mb7)
#          in
#            x1;
#            let (mulA, mb9) =
#              (3, mb8)
#            in
#              let (mulB, mb10) =
#                (4, mb9)
#              in
#                let (x2, mb11) =
#                  (serverPid ! Mul(mb10, mulA, mulB), mb10)
#                in
#                  x2;
#                  let (a, mb12) =
#                    guard mb11: Ans.Ans {
#                      receive Ans(a) from mb13 ->
#                        (a, mb13)
#                    }
#                  in
#                    let (b, mb14) =
#                      guard mb12: Ans {
#                        receive Ans(b) from mb15 ->
#                          (b, mb15)
#                      }
#                    in
#                      (a + b, mb14)
#}

#def main(mb0: ClientMb?): (Int * ClientMb?) {
#
#    let (serverPid, mb1) =
#      (let mb2 =
#        new [ServerMb]
#      in
#        let x0 =
#          spawn { server(mb2) }
#        in
#          (mb2, x0)) # Unit
#    in
#      mb1;
#      let (self1, self1) =
#        (mb0, mb0)
#      in
#        let (addA, mb3) =
#          (1, self1)
#        in
#          let (addB, mb4) =
#            (2, mb3)
#          in
#            let (x1, mb5) =
#              (let (x2, mb00) =
#                (serverPid, mb4)
#              in
#                (x2 ! Add(mb00, addA, addB), mb00))
#            in
#              x1;
#              let (self2, self2) =
#                (mb5, mb5)
#              in
#                let (mulA, mb7) =
#                  (3, self2)
#                in
#                  let (mulB, mb8) =
#                    (4, mb7)
#                  in
#                    let (x3, mb9) =
#                      (let (x4, mb01) =
#                        (serverPid, mb8)
#                      in
#                        (x4 ! Mul(mb01, mulA, mulB), mb01))
#                    in
#                      x3;
#                      let (a, mb10) =
#                        guard mb9: Ans.Ans {
#                          receive Ans(a) from mb11 ->
#                            (a, mb11)
#                        }
#                      in
#                        let (b, mb12) =
#                          guard mb10: Ans {
#                            receive Ans(b) from mb13 ->
#                              (b, mb13)
#                          }
#                        in
#                          (5, mb12)
#}

#def add(mb0: ClientMb?, a: Int, b: Int): (Int * ClientMb?) {
#  (a + b, mb0)
#}


#def main(mb0: ClientMb?): (Int * ClientMb?) {
#  let (serverPid, mb1) =
#    (let mb2 =
#      new [ServerMb]
#    in
#      let x0 =
#        spawn { server(mb2) }
#      in
#        (mb2, x0)) # Unit
#  in
#    mb1;
#    let (xx, self1) =
#      ((), mb0)
#    in
#      xx;
#      let (addA, mb3) =
#        (1, self1)
#      in
#        let (addB, mb4) =
#          (2, mb3)
#        in
#          let (x1, mb5) =
#            (let (x2, mb00) =
#              (serverPid, mb4)
#            in
#              (x2 ! Add(mb00, addA, addB), mb00))
#          in
#            x1;
#            (5, mb5)
#}

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
    let (x1, self) =
      ((), mb0) # alias = self to be replaced with the current mailbox. Translation of self = ((), current_mb) because of the unable to synthesise error
    in
      x1; # Use unit value.
      let (addA, mb3) =
        (1, self)
      in
        let (addB, mb4) =
          (2, mb3)
        in
          let (x2, mb5) =
            (let (x3, mb6) =
              (serverPid, mb4) # Does not work because of this with error: [CONSTRAINT GENERATION (Join)] Mailbox variable serverPid was used after being consumed by a 'guard' or a 'let' binding.
            in
              #x3;
              (x3 ! Add(mb6, addA, addB), mb6)) # alias = self replaced with the current mailbox mb6
          in
            x2; # Use unit value.
            let (mulA, mb7) =
              (3, mb5)
            in
              let (mulB, mb8) =
                (4, mb7)
              in
                let (x4, mb9) =
                  (let (x5, mb10) =
                    (serverPid, mb8) # Does not work because of this with error: [CONSTRAINT GENERATION (Join)] Mailbox variable serverPid was used after being consumed by a 'guard' or a 'let' binding.
                  in
                    (x5 ! Mul(mb10, mulA, mulB) , mb10)) # alias = self replaced with the current mailbox mb10
                in
                  x4; # Use unit value.
                  let (a, mb11) =
                    guard mb9: Ans.Ans {
                      receive Ans(a) from mb12 ->
                        (a, mb12)
                    }
                  in
                    let (b, mb13) =
                      guard mb11: Ans {
                        receive Ans(b) from mb14 ->
                          (b, mb14)
                      }
                    in
                    (a + b, mb13)
}