(*
  Given A list of pairs, return the first pair of lists
*)
let fst_pair_lists list = fst (List.split list)

(*
  Is the input a character?
*)
let is_char str = String.length str = 1


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