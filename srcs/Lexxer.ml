open Types
module CharSet = Types.CharSet
module StringSet = Types.StringSet

let _tokenize_symbols jsonobj = 
  let inputlist = Utils.json_to_list ~name:"alphabet" (Utils.get_member jsonobj "alphabet") in
  let stringlist = Yojson.Basic.Util.filter_string inputlist in 
  (*
    Make sure all characters and unqiue, have length > 0, and only made of 1 byte characters
  *)
  if Core.List.contains_dup stringlist ~compare:String.compare then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} Usage: ./ft_turing Alphabets need to be unique\n"
  end else if List.length stringlist = 0 then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} Usage: ./ft_turing Alphabets need to have at least 1 alphabet\n"
  end else if ((List.for_all Utils.is_char stringlist) = false) then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} Usage: ./ft_turing Alphabets should be 1 byte characters\n"
  end;
  let char_list = List.map (fun str -> String.get str 0) stringlist in
  CharSet.of_list char_list

let _tokenize_blank jsonobj alphabet = 
  let inputstr = Utils.json_to_str ~name:"blank" (Utils.get_member jsonobj "blank") in
  let blank = String.get inputstr 0 in
  (*
    Make sure blank is just 1 character and does not collide with alphabet
  *)
  if String.length inputstr != 1 then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} Usage: ./ft_turing Blank needs to have at exactly 1 alphabet\n"; exit (-1)
  end else if not (CharSet.mem blank alphabet) then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} Usage: ./ft_turing Blank must be part of alphabet\n"; exit (-1)
	end;
  blank

let _tokenize_states jsonobj = 
  let inputlist = Utils.json_to_list ~name:"states" (Utils.get_member jsonobj "states") in
  let stringlist = Yojson.Basic.Util.filter_string inputlist in 
  (*
    Make sure all states and unqiue, have length > 0
  *)
  if Core.List.contains_dup stringlist ~compare:String.compare then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} Usage: ./ft_turing States need to be unique\n"
  end else if List.length stringlist = 0 then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} Usage: ./ft_turing States need to have at least 1 alphabet\n"
  end;
  stringlist

let _tokenize_initial jsonobj states = 
  let initial = Utils.json_to_str ~name:"initial" (Utils.get_member jsonobj "initial") in
  (*
    Make sure initial is in states
  *)
  if not (StringSet.mem initial states) then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} Usage: ./ft_turing Initial need to be part of states\n"
	end;
  initial

let _tokenize_finals jsonobj states initial = 
  let inputlist = Utils.json_to_list ~name:"finals" (Utils.get_member jsonobj "finals") in
  let stringlist = Yojson.Basic.Util.filter_string inputlist in 
  (*
    Make sure finals is in states, no duplicates and does not collide with initials
  *)
  List.iter (
    fun final -> if not (StringSet.mem final states) then begin
      Spectrum.Simple.printf "@{<red>[ERROR]@} Usage: ./ft_turing Final %s need to be part of states\n" final; exit (-1)
    end else if String.compare final initial = 0 then begin
      Spectrum.Simple.printf "@{<red>[ERROR]@} Usage: ./ft_turing Final %s needs to be different from initial\n" final; exit (-1)
    end
      ) stringlist;
    if List.length stringlist = 0 then begin
      Spectrum.Simple.printf "@{<red>[ERROR]@} Usage: ./ft_turing Finals must not be 0 length\n"; exit (-1)
    end else
    if Core.List.contains_dup stringlist ~compare:String.compare then begin
      Spectrum.Simple.printf "@{<red>[ERROR]@} Usage: ./ft_turing Finals need to be unique\n"; exit (-1)
    end;
  StringSet.of_list stringlist

let _verify_transitions transitions_assoc states finals =
  let members = Utils.fst_pair_lists transitions_assoc in
  let transition_names = StringSet.of_list members in 
  let compared_set = StringSet.diff states finals in
  if not (StringSet.equal compared_set transition_names) then begin 
    Spectrum.Simple.printf "@{<red>[ERROR]@} Usage: ./ft_turing ALL transition names must be states except finals\n"; exit (-1)
  end else if Core.List.contains_dup members  ~compare:String.compare then begin
    Spectrum.Simple.printf "@{<red>[ERROR]@} Usage: ./ft_turing transitions must be unique\n"; exit (-1)
  end

let _tokenize_transitions json states finals=
  let jsonobj = Utils.get_member json "transitions" in
  let transitions_assoc = Utils.json_to_associate ~name:"transitions" jsonobj in
  _verify_transitions transitions_assoc states finals;
  transitions_assoc

let tokenize_input () =
  (* Check argv *)
  if (Array.length Sys.argv) != 3 then
    (Spectrum.Simple.printf "@{<red>%s@} Usage: ./ft_turing <path_to_machine> <input>\n" "[ERROR]"; exit(-1))
  else  
    (*
      Read file of machine definition
      returns: Yojson json object
    *)
    let json =  try Yojson.Basic.from_file Sys.argv.(1) with
    | Yojson.Json_error str -> Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing : %s : %s\n" Sys.argv.(1) str ; exit (-1)
    | Sys_error str ->  Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing : %s\n" str ; exit (-1)
    | _ ->  Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing : %s : Error opening json file\n" Sys.argv.(1) ; exit (-1)
    in

    (*
      Extract the items of the JSON object
      returns : (string * Yojson.Basic.t) list
    *)
    let assoc = try Yojson.Basic.Util.to_assoc json with
	  | _ -> Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing : %s must be a list\n" Sys.argv.(1) ; exit (-1)
    in

    (*
      Check for required items, dupes and unknowns JSON object
      returns : unit
    *)
    let required_members = ["name"; "alphabet"; "blank"; "states"; "initial"; "finals"; "transitions"] in
    let members = Utils.fst_pair_lists assoc in
    Utils.confirm_members_exist members required_members;
    if Core.List.contains_dup members ~compare:String.compare then begin
      Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing : duplicate members\n" ; exit (-1)
    end else if not (Core.List.equal (fun s1 s2 -> (String.compare s1 s2 = 0))  members required_members) then begin
      Spectrum.Simple.printf "@{<red>[ERROR]@} ft_turing : unknown members\n" ; exit (-1)
    end;

    (*
      Extract all individual items from validated items
      returns : unit
    *)
    let name = Utils.json_to_str (Utils.get_member json "name") in
    let symbols = _tokenize_symbols json in 
    let blank = _tokenize_blank json symbols in 
    let stateslst = _tokenize_states json in
    let states = StringSet.of_list stateslst in
    let initial = _tokenize_initial json states in 
    let finals = _tokenize_finals json states initial in
    let transitions = _tokenize_transitions json states finals in


    (*
      Construct the machine struct
    *)
    let data = {
      name = name;
      alphabet = symbols;
      blank = blank;
      stateslst = stateslst;
      states = states;
      initial = initial;
      finals = finals;
      transitions = transitions
    } in data 