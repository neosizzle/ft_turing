open Types

let _get_fst (a,_,_,_) = a
let _get_snd (_,a,_,_) = a
let _get_trd (_,_,a,_) = a
let _get_fou (_,_,_,a) = a

let _find_transition_by_tape_char tape_char transitions =
  Array.find_index (fun x ->
    match x with
    | Defined(trans) -> (_get_fst trans) = tape_char
    | Undefined -> false
    ) transitions

let _get_state_index_by_state_str transition_table input = 
  Array.find_index (fun elem -> match elem with
  | Normal(state_name, _) -> input = state_name
  | Final(state_name) -> input = state_name
  ) transition_table

(* Iterate with new data *)
let rec execute tape (machine:machine_definition)  = 
  (* Print the current tape *)
  let tape_str = Tape.tape_to_str tape in
  Spectrum.Simple.printf "@{<grey>%s@}\n" tape_str;

  (* Get current state *)
  (* returns  state_transitions array *)
  let curr_state = machine.transition_table.(machine.current_state) in 

  
  match curr_state with 
  (* Base Case: Current state is a halt state *)
  | Types.Final state_str -> state_str ^ "\n"
  | Types.Normal(_transitions) -> 
    (* Read character at tape and get transition from machine defs*)
    let tape_char = Tape.read_head tape in

    (* Lookup transition *)
    let is_transition_found = _find_transition_by_tape_char tape_char (snd _transitions) in
    match is_transition_found with
    | None -> Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing: Executor::execute: transition not even found"; exit(-1)
    | Some idx -> match (snd _transitions).(idx) with
        | Types.Defined(transition) ->
          (* Transition is found, commemerate actions.. *)
          (* Write write_char to current state of tape*)
          let written_tape = Tape.write_head tape (String.make 1 (_get_snd transition)) in

          (* Move tape direction *)
          let moved_tape = match (_get_trd transition) with
          | Types.Left -> Tape.left written_tape
          | Types.Right -> Tape.right written_tape in

          (* Get new state index *)
          let new_state_idx = match _get_state_index_by_state_str machine.transition_table (_get_fou transition) with
          | Some(idx) -> idx 
          | None -> begin Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing: Executor::execute: Cant find new state index\n"; exit (-1) end in

          (* Create new machine definition with updated state_index *)
          let new_machine = {machine with current_state = new_state_idx } in

          execute moved_tape new_machine
        | Types.Undefined -> "undefined!!!"