### NOTE: This works only with 'mbcheck' compiled from the 'paterl-experiments'
###       branch.

### Interface definitions.

interface Dummy { M() }

# This is a small program that works after the disabling quasi-linearity checking
# in Pat. Note that it does not require the precise typing constraints to be
# explicitly specified after ! and ?.
def self(mb: Dummy?1): (Dummy! * Dummy?) {
    (mb, mb)
}

def main(): Unit {
    let x = new[Dummy] in
    let (a, b) = self(x) in
    let a' : Dummy!1 = a in # This is required to provide the type-checker with the information that a has type Dummy!1
    free(b)
}

# This is also a small program that works where self is defined inline via a let
# expression whose types are explicitly specified, enabling Pat to stay in
# checking mode and determine the types of the variables (x, x).


# The alternative to the above is specifying the type information of the mailbox
# in-line with let. The let expression specifies that the first component of
# the pair, x, has type Dummy!, whereas the second pair, x, has type Dummy?.
# The specific type after the ! and ? is inferred automatically. We could have
# also written (Dummy!1 * Dummy?1) and it would have also been correctly
# inferred.
def main2(): Unit {
    let x = new[Dummy] in
    let (a, b) : (Dummy! * Dummy?) = (x, x) in
    let y = () in
    free(b)
}

# Use after free.
def main(): Unit {
    let a = new[Dummy] in # Type of mb is Dummy!1.
    guard a : M { # Using mb here at type Dummy?M.
        receive M() from a -> free(a) # Using mb here at type Dummy?1.
    };
    a ! M()
}

# Self-deadlock which right now requires the last message M to be sent. Can this
# be implemented in a way that it does not send but still self-deadlocks?
def main8(): Unit {
  let mb = new [Dummy] in # Type of mb is Dummy!1.

  guard mb: M { # Using mb here at type Dummy?M.
    receive M() from mb1 -> free(mb1) # Using mb here at type Dummy?1.
  };
  mb ! M() # CAN WE REMOVE THIS LINE AND STILL SELF-DEADLOCK?
}