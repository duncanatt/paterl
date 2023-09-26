def int_pair(): (Int * Int) {
    (5, 5)
}

def main(): Unit {
    let (val0, val1) = int_pair() in
    #print(intToString(val0));
    #print(intToString(val1));
    ()

    # When I do not use val0 and val1, I get the following error:
    # [CONSTRAINT GENERATION (Synth)] Cannot synthesise type for variable : Only able to synthesise
    #             base variable types
    # Cannot understand why exactly. Would have expected an error about not using val0 and val1.
}

main()