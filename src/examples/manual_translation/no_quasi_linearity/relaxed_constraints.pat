### NOTE: This works only with 'mbcheck' compiled from the 'paterl-experiments'
###       branch.

### Interface definitions.

interface Dummy { M() }


## CONTINUE FROM HERE.
# If the return value of the function is Dummy?M, Pat would complain since the
# mailbox is expected to be a receiving mailbox that contains the message M in
# order to balance out the sent and receive messages M.
# This is fixed sending M to mb, which leaves it in the state where it contains
# M, which has type Dummy?M.
#
# If the return value of the function is Dummy!M, Pat would complain since the
# mailbox is expected to be a sending mailbox that must receive the message M in
# order to balance out the send and receive messages M.
# This is fixed by receiving M from mb, which leaves it in the state where it
# expects M, which has type Dummy!M.
#
# The thing to keep in mind is that when analysing functions on their own, the
# resultant state of the mailbox in that function should be balanced. This is
# achieved either by modifying the types of the parameters and return values
# or the code in the function itself. The aim is to tweak these to balance the
# mailbox out.
def main6(): Dummy?1 {
  new [Dummy]
}


# A new mailbox has the balanced receive type Dummy?1.
def main(): Dummy?1 {
  let mb:Dummy?1 = new [Dummy] in mb
}

# Question. Do we need to specify the message M in the mailbox type? Maybe or
# maybe not, depends on the rest of the program. But I'll have to check this.

# Mailbox mb starts at receive type Dummy?1, which is balanced. One way to keep
# mb balanced in the pair (mb, mb) is to use types Dummy!M and Dummy?M together,
# which balance out.
def main(mb: Dummy?1): (Dummy!M * Dummy?M) {
   (mb, mb) # Both mb are at type Dummy?1
}

# Mailbox mb starts at receive type Dummy?1, which is balanced. Another way to
# keep mb balanced in the pair (mb, mb) is to add (!) and remove (?) nothing,
# using types Dummy!1 and Dummy?1.
def main(mb: Dummy?1): (Dummy!1 * Dummy?1) {
  (mb, mb)
}

# Mailbox mb starts at receive type Dummy?M, which is imbalanced. One way to
# redress this balance in the pair (mb, mb) is to use types Dummy!M and
# Dummy?(M.M). Message M in Dummy!M balances out one message M from Dummy?(M.M).
# The other message M of type Dummy?(M.M) accounts for the imbalance in the
# original mailbox the function accepts as a parameter, which starts at type
# Dummy?M.
def main(mb: Dummy?M): (Dummy!M * Dummy?(M.M)) {
  (mb, mb)
}

# Mailbox mb starts at receive type Dummy?M, which is imbalanced. Another way to
# redress this balance in the pair (mb, mb) is to use types Dummy!1 and Dummy?M.
# Dummy!1 does not add messages to mb, whereas Dummy?M accounts for the
# imbalance in the original mailbox the function accepts as a parameter, which
# starts at type Dummy?M and coincides with the return type Dummy?M.
def main(mb: Dummy?M): (Dummy!1 * Dummy?M) {
  (mb, mb)
}

# This works for the same reasons given above.
def main(mb: Dummy?(M.M)): (Dummy!(M.M) * Dummy?(M.M.M.M)) {
  (mb, mb)
}

# Question. What about starting out with Dummy!1 or Dummy!M. Does it even make
# sense?

# Why does not this work?
#def main_not_work(mb: Dummy!1): (Dummy!M * Dummy?M) {
#  (mb, mb)
#}

# Question. Do we need to specify the messages M in the type, or can these be
# inferred in some way?

# The new mailbox mb starts at receive type Dummy?M, which is balanced. The send
# adds one message M to mb, which is returned by the function. The return type
# accounts for the imbalance via the type Dummy?M.
# The return type can also be Dummy? in this case, and Pat infers the rest.
def main(): Dummy?M {
  let mb = new [Dummy] in # mb is at type Dummy?1.
  mb ! M(); # Using mb at type Dummy!M.
  mb # mb is at type Dummy?M.
}

# Ditto.
def main(): Dummy?M {
  let mb = new [Dummy] in # mb is at type Dummy?1.
  spawn {
    mb ! M() # Using mb at type Dummy!M.
  };
  mb # mb is at type Dummy?M.
}

def main4(): Dummy?M { # Or also Dummy? and Pat will infer Dummy?M
  let mb = new [Dummy] in # Type of mb is Dummy?.
  spawn { mb ! M() }; # Use mb to send message, which makes mb of type Dummy!.
  mb # Type of mb is Dummy?M
}

# Ditto.
def main(): Dummy?M {
  let mb = new [Dummy] in # mb is at type Dummy?1.
  spawn {
    let mb1:Dummy!M = mb in # mb1 has type Dummy!M because mb is used at type Dummy!M in the body of let.
    mb1 ! M() # Using mb at type Dummy!M.
  };
  mb # mb is at type Dummy?M
}

# Still need to figure out why it works. It is because of spawn, but I need to
# see the paper. But confirm with Simon. I would have expected to use the same
# reasoning as above, i.e. "mb1 has type Dummy!M because mb is used at type Dummy!M in the body of let."
# but I am wrong.
def main(): Dummy?M {
  let mb = new [Dummy] in
    let mb1:Dummy?1 = mb in # I don't understand this bit here! Why is it not Dummy!M ??
    mb1 ! M();
    mb1
}





# Make one with a spawned Process which receives.








def main5(): Dummy!M {
  let mb:Dummy?1 = new [Dummy] in # Type of mb is Dummy?.
  spawn { guard mb: M { receive M() from mb2 -> free(mb2) } }; # Guard on mb, which must be of type Dummy?.
  #mb ! M(); # Type of mb is Dummy! This line is also needed to re-balance the mailbox.
  mb
}

# Once the process in the spawn reads from the mailbox, it will always be a ? side
# and the mb on the spawner side will always be a ! side. In fact, look at the
# return value. If the return value was ?, it would mean that two different
# processes can read from the same mailbox (i.e. message reception can be
# delegated, which is obviously not possible).
def main5(): Dummy!1 {
  let mb:Dummy?1 = new [Dummy] in # Type of mb is Dummy?.
  spawn { guard mb: M { receive M() from mb2 -> free(mb2) } }; # Guard on mb, which must be of type Dummy?.
  mb ! M(); # Type of mb is Dummy! This line is also needed to re-balance the mailbox.
  mb
}

def main5(): Dummy!(M.M) {
  let mb:Dummy?1 = new [Dummy] in # Type of mb is Dummy?.
  spawn {
    guard mb: M.M { receive M() from mb -> guard mb: M  { receive M() from mb -> free(mb) } }
  }; # Guard on mb, which must be of type Dummy?(M.M)
  mb # Type of mb is Dummy!(M.M) because:
     # (1) the spawned process receives, so its mailbox is now forever a ? end,
     #     and the returned mb in the parent process must be a !, and
     # (2) the spawned process receives M() twice, so to re-balance it, the mb
     #     in the parent process must send M() twice.
}

def main5(): Dummy!(M) {
  let mb:Dummy?1 = new [Dummy] in # Type of mb is Dummy?.
  spawn {
    guard mb: M { receive M() from mb -> free(mb) }
  }; # Guard on mb, which must be of type Dummy?M.
  mb # Type of mb is Dummy!() because:
     # (1) the spawned process receives, so its mailbox is now forever a ? end,
     #     and the returned mb in the parent process must be a !, and
     # (2) the spawned process receives M() once, so to re-balance it, the mb
     #     in the parent process must send M() once.
}

def main5(): Dummy!(M) {
  let mb:Dummy?1 = new [Dummy] in # Type of mb is Dummy?.
  spawn {
    guard mb: M.M { receive M() from mb -> guard mb: M  { receive M() from mb -> free(mb) } }
  }; # Guard on mb, which must be of type Dummy?M.
  mb ! M(); # Type of mb is Dummy! This line is also needed to re-balance the mailbox.
  mb # Type of mb is Dummy!(M) because:
     # (1) the spawned process receives, so its mailbox is now forever a ? end,
     #     and the returned mb in the parent process must be a !, and
     # (2) the spawned process receives M() twice and to ensure that the function
     #     return type Dummy!(M) is met, the parent process must send M() once.
}


