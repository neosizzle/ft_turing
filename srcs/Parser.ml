(* 
let () = let my_string = "such a beautiful string" in
logger#sinfo my_string; *)

let json_string = {|
  {"number" : 42,
   "string" : "yes",
   "list": ["for", "sure", 42]}|}
let json = Yojson.Safe.from_string json_string

let hello () =
  if (Array.length Sys.argv) != 3 then
    raise (Failure "Usage: ./ft_turing <path_to_machine> <input>")
  else  
    (* let json_str = Yojson.Safe.from_string Sys.argv.(1) in *)
    (* Printf.printf "%s\n" Sys.argv.(1) *)
    Logger.logger#sinfo  Sys.argv.(1);