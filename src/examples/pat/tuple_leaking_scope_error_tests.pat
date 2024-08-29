## Scoping issues.

interface Empty { Msg(Int) }

# Works.
def let_tuple_ok(x: String): (Int * String) {
  (let y = 10 in y, x)
}

# Fails. I may seem that renaming the bound variable would fixes it. But the
# next example shows that it is a scoping issue.
def let_tuple_bad(x: String): (Int * String) {
  (let x = 10 in x, x)
}

# Works but should fail since x in the second tuple element is out of scope.
#def let_tuple_bad2(): (Int * Int) {
#  (let x = 5 in x, let y = "" in x)
#}

# Works but should fail for y, as above.
#def let_tuple_bad3(): (Int * Int) {
#  (let x = 5 in x, let y = 10 in x + y)
#}


## Unable to synthesise types with tuples.

# Works.
def synth_ok(): Unit {
  let x =
    5
  in
    let y =
      10
    in
      ()
}

# Works.
def synth_ok2(): Unit {
  let (x, y) =
    ((), ()) # Same for integers, etc.
  in
    x;
    y
}

# Is the problem tuple-related?
# Fails. Needs y to be present as well.
def synth_bad(): Unit {
  let (x, y) =
    ((), ()) # Same for integers, etc.
  in
    x # Need y as well.
}

# Works.
#def synth_ok3(): Unit {
#  let (x, y): (Int * Unit) =
#    (5, ())
#  in
#    ()
#}

## Branching.

# Works.
def if_ok(): Int {
  if (true) {5} else {10}
}

# Works. Type error detected.
#def if_ok2(): Int {
#  let x = ""
#    in
#  if (true) {let x = 5 in x} else {x}
#}

# Is it possible to allow literals as function parameters?

def simple(): Int {
  let outer_var: Empty! =
    (let mb0 =
      new [Empty]
    in
      let y =
        spawn {
          let mb1 =
            x(mb0)
          in
            free(mb1)
          }
      in
        mb0)
    in
      #outer_var ! Msg(5);
      5
}

def x(mb0: Empty?): Empty? {
	#guard mb0: Msg {
	#  receive Msg(a) from mb1 ->
	#    mb1
	#}
	mb0
}