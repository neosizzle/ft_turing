
let () =
let machine = Machinebuilder.build(Lexxer.tokenize_input()) in
let tape = Tape.create machine Sys.argv.(2) in
let end_state = Executor.execute tape machine in 
print_endline end_state