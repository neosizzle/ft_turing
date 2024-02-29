open Types

(* Create a tape struct based on machine definition and input *)
let create (machine_definition:Types.machine_definition) input = 
  String.iter (
    fun (curr_char) ->
      if not (CharSet.mem curr_char machine_definition.alphabet) then begin
        if curr_char != machine_definition.blank then begin
          Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing: Tape::create: %c is an invalid input\n" curr_char; exit (-1)
        end
      end
  ) input;
  {
    blank = machine_definition.blank;
    data = input;
    head = 0;
    size = String.length input
  }

(* Given a tape struct, read the data at its head *)
let read_head tape = String.get tape.data tape.head

(* Given a tape struct and an index, read the data at index from tape *)
let read_tape_i tape i = String.get tape.data i

(* Given a tape struct, write the data to the head of the tape*)
let write_head tape newchar = 
  let new_data = Utils.replace_char tape.head tape.data newchar in 
  { tape with data = new_data }

(* Move head to the left on tape, but add new blank to data if size overflows *)
let left tape =
  if tape.head - 1 >= 0 then { tape with head = tape.head - 1 }
  else { tape with head = 0 ; size = tape.size + 1 ; data = ((String.make 1 tape.blank) ^ tape.data) }

(* Move head to the right on tape, but add new blank to data if size overflows *)
let right tape =
  if tape.head + 1 < tape.size then { tape with head = tape.head + 1 }
  else  { tape with head = tape.size ; size = tape.size + 1 ; data = (tape.data ^ (String.make 1 tape.blank) ) }

(* Converts the tapes data to string, with the head position *)
let tape_to_str tape = 
  let rec _loop i res =
    if i < tape.size then
      if i = tape.head then
        (* _loop (i + 1) (res ^ "@{<red>G@}" ^ (Char.escaped (read_tape_i tape i)) ^ "]") *)
        _loop (i + 1) (Spectrum.Simple.sprintf ("%s@{<green>[@}%s@{<green>]@}@{<grey>") res (Char.escaped (read_tape_i tape i)))
      else 
        _loop (i + 1) (res ^ (Char.escaped (read_tape_i tape i)))
    else
      res 
    in
  _loop 0 ""