
let () =
let machine = Machinebuilder.build(Lexxer.tokenize_input()) in
let tape = Tape.create machine Sys.argv.(2) in
let state_cache = StateCache.create in
 
(* let test_hash = StateCache.hash "test-state" tape machine.blank in
print_endline test_hash *)

let end_state = Executor.execute tape state_cache machine in 
print_endline end_state