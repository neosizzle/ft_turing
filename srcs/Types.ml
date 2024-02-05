
(* A set containing characters only, uses Stdlib compare *)
module CharSet = Set.Make (
struct
	type t = char
	let compare = Stdlib.compare
end)

(* A set containing strings only, uses String compare *)
module StringSet = Set.Make (
struct
	type t = string
	let compare = String.compare
end)

(* Machine configuration *)
type machine_config = {
	name : string;
	alphabet : CharSet.t;
	blank : char;
	stateslst : string list;
	states : StringSet.t;
	initial : string;
	finals : StringSet.t;
	transitions : (string * Yojson.Basic.t) list;
}

(* Machine definition *)
type action = Left | Right
(* type action = string *)
type state_index = int
type to_state = string
type read_char = char
type write_char = char
type transition = Defined of (read_char * write_char * action * to_state) | Undefined (*Two possible values, can be Defined aka (char * action * state_idx) or Undefined aka unit*)
type state_transitions = Normal of (string * transition array) | Final of string (*Two possible values, can be Normal aka (string * transition array) or Final aka String*)

type machine_definition = {
	name : string;
	alphabet : CharSet.t;
	blank : char;
	transition_table : state_transitions array;
	current_state : state_index;
}

(* Represents the machine's tape / input / memory *)
type tape = {
	blank: char;
	data: string;
	head: int;
	size: int;
}

(* Represents a state cache entry *)
type cache_entry = {
	hash: string;
	hits: int;
	state: string;
	tape: tape;
}

(* Represents a state cache to store snapshots of states *)
type state_cache = {
	entries: cache_entry list;
	total_hits: int;
	hit_limit: int;
}