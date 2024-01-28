
let () = let machine = Machinebuilder.build(Lexxer.tokenize_input()) in Tape.create machine "input" in ()