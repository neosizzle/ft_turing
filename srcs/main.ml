
let () =
let machine = Machinebuilder.build(Lexxer.tokenize_input()) in
let tape = Tape.create machine "input" in
let newtape = Tape.left tape in
let tape_str = Tape.tape_to_str newtape in
print_string tape_str ;
()