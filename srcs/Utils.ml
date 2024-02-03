open Types

(*
  Given A list of pairs, return the first pair of lists
*)
let fst_pair_lists list = fst (List.split list)

(*
  Is the input a character?
*)
let is_char str = String.length str = 1

(*
  Are the strings equal?
*)
let str_equal s1 s2 = String.compare s1 s2 = 0

(*
  First n characters
*)
let first_n_chars n str = 
  String.sub str 0 n
   
(*
  Last n characters
*)
let last_n_chars n str = 
  String.sub str ((String.length str) - n) n

(*
  Replace a character with _chr of str at index n
*)
let replace_char n str _char = 
  if n < 0 then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing: replace_char: negative index\n" ; exit (-1)
  end else if n > String.length str then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing: replace_char: index too big\n" ; exit (-1)
  end;
  let first_half = first_n_chars n str in
  let sec_half = last_n_chars ((String.length str) - n - 1) str in
  first_half ^ _char ^ sec_half

(*
  Confirm that curr_members have at least all members in required_members
*)
let rec confirm_members_exist curr_members required_members = 
  match required_members with
  | curr_match::rest when List.exists (fun _match ->  String.compare curr_match _match = 0) curr_members -> confirm_members_exist curr_members rest
  | curr_match::rest ->  Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing : %s is missing from list\n" curr_match ; exit (-1)
  | [] -> ()

(*
  Gets member from json object
  returns : Yojson.Basic.t
*)
let get_member json name =
	let jobj = Yojson.Basic.Util.member name json in
	match jobj with
	| `Null -> Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing : %s is missing not in json\n" name ; exit (-1)
	| _ -> jobj

(*
  Converts a json object to string
*)
let json_to_str ?(name = "") json = 
  try Yojson.Basic.Util.to_string json with
	| _ -> Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing : member %s is not a string\n" name ; exit (-1)

(*
  Converts a json object to list
*)
let json_to_list ?(name = "") json = 
  try Yojson.Basic.Util.to_list json with
	| _ -> Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing : member %s is not a list\n" name ; exit (-1)

(*
  Converts a json object to an associate
*)
let json_to_associate ?(name = "") json = 
  try Yojson.Basic.Util.to_assoc json with
	| _ -> Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing : member %s is not a list\n" name ; exit (-1)

(*
  Prints a machine definition
*)
let print_machine_definition machine = 
  Spectrum.Simple.printf "@{<green>@{<bold>===============@}@} @{<aqua>%s@} @{<green>@{<bold>===============@}@}\n" machine.name ;
  Spectrum.Simple.printf "@{<green> Blank: @} %c\n" machine.blank ;
  Spectrum.Simple.printf "@{<green> Alphabets: @} " ;
  Types.CharSet.iter (fun elem -> Spectrum.Simple.printf " '%c' " elem ) machine.alphabet ;
  Spectrum.Simple.printf "\n@{<green> Current state: @} %d\n" machine.current_state;
  Spectrum.Simple.printf "@{<green> Transitions: @} \n" ;
  let print_transition transition =
    match transition with 
    | Defined (a, b, c, d) ->  Spectrum.Simple.printf "\t@{<aqua>Read char :@} %c\n\t@{<aqua>Write char :@} %c\n\t@{<aqua>Action :@} %s\n\t@{<aqua>ToState :@} %s\n\n" a b (match c with | Left -> "Left" | _ -> "Right") d;
    | Undefined ->  Spectrum.Simple.printf "\t[UNDEFINED]\n\n"
   

  in
  let print_state_transitions transition = 
    match transition with
    | Normal (state_name, transitions) -> Spectrum.Simple.printf "  @{<blue> %s: @}\n" (state_name); 
      Array.iter print_transition transitions;
    | Final state_name -> Spectrum.Simple.printf "  @{<blue> %s: @} [FINAL]\n" (state_name)
  in

  Array.iter (fun (pair:(Types.state_transitions)) -> print_state_transitions pair) machine.transition_table;
  Spectrum.Simple.printf "@{<green>@{<bold>===============@}@} Execution @{<green>@{<bold>===============@}@}\n";
  