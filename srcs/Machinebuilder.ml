open Types
module CharSet = Types.CharSet
module StringSet = Types.StringSet

let _get_action action = if Utils.str_equal action "LEFT" then Left else Right
let _get_index lst elem =
	let rec loop lst i = match lst with
	| head::tail when Utils.str_equal head elem -> i
	| head::tail -> loop tail (i + 1)
	| [] -> raise Not_found
	in loop lst 0

(* create an individual transition object based on transition json *)
let _extract_transition_and_push index transition_array (configuration: machine_config)  transition_json =
  (* Validate required members *)
  let required_members = ["read"; "to_state"; "write"; "action"] in
  let assoc = Utils.json_to_associate transition_json in
  Utils.confirm_members_exist (Utils.fst_pair_lists assoc) required_members;

  (* Extracts and validtes individual transition values *)
  let read = Utils.json_to_str (Yojson.Basic.Util.member "read" transition_json) ~name:"read" in
  let to_state = Utils.json_to_str (Yojson.Basic.Util.member "to_state" transition_json) ~name:"to_state"  in
  let write = Utils.json_to_str (Yojson.Basic.Util.member "write" transition_json) ~name:"write"  in
  let action = Utils.json_to_str (Yojson.Basic.Util.member "action" transition_json) ~name:"action"  in

  if not (Utils.is_char read) then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing: read value must be a char\n"; exit(-1)
  end else if not (CharSet.mem (String.get read 0) configuration.alphabet) then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing: read value must be a part of the alphabet\n"; exit(-1)
  end else if (Array.get transition_array index <> Undefined) then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing: multiple transitions are associated with this index (%d)\n" index; exit(-1)
  end else if not (Utils.is_char write) then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing: write value must be a char\n"; exit(-1)
  end else if not (CharSet.mem (String.get write 0) configuration.alphabet) then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing: write value must be a part of the alphabet\n"; exit(-1)
  end else if not (StringSet.mem to_state configuration.states) then begin
    (Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing: [%s] must be part of states\n", to_state); exit(-1);
  end else if (not (Utils.str_equal action "LEFT")) && (not (Utils.str_equal action "RIGHT")) then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing: action must be LEFT or RIGHT\n"; exit(-1)
  end ; 

  (* Packs the values up and add it to array *)
  Array.set transition_array index (Types.Defined(String.get read 0, String.get write 0, _get_action action, to_state))

(* Builds a normal state type based on the transition array given*)
let _build_normal_state (transition_state, transition_array) configuration =
  (* Convert JSON data to a list *)
  let transition_json_list = Utils.json_to_list ~name:transition_state transition_array in

  (* Create new result array with length of list above *)
  let parsed_transition_array = Array.make (List.length transition_json_list) Types.Undefined in 

  (* For every transition of a state, create an individual transition object*)
  List.iteri (fun idx elem -> _extract_transition_and_push idx parsed_transition_array configuration elem) transition_json_list ;
  Types.Normal((transition_state, parsed_transition_array))

(* Builds a machine_definition, mainly constructing the transition tables and 
individual states here *)
let build configuration = 
  (* Initialize an empty state transiton table *)
  let transition_table = Array.make (List.length configuration.stateslst) (Types.Final("__EMPTY__")) in

  (* Define a function that pushes to the state_table *)
  let add_to_state_table index state_name = 
    if StringSet.mem state_name configuration.finals then
      (* If state name is in the finals list, create a final state type *)
      Array.set transition_table index (Types.Final(state_name))
    else 
      (* Run another function to build the normal state and its transitions *)
      Array.set transition_table index (_build_normal_state (List.find (fun (str, _ ) -> Utils.str_equal state_name str) configuration.transitions) configuration) in

  (* Use the function created above on all states in configuration *)
  List.iteri add_to_state_table configuration.stateslst;
  let machine = {
    name = configuration.name;
    alphabet = configuration.alphabet;
    blank = configuration.blank;
    transition_table = transition_table;
    current_state = _get_index configuration.stateslst configuration.initial
  } in 
  Utils.print_machine_definition machine ;
  machine