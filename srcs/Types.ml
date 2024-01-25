
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
type parsing = {
	name : string;
	alphabet : CharSet.t;
	blank : char;
	stateslst : string list;
	states : StringSet.t;
	initial : string;
	finals : StringSet.t;
	transitions : (string * Yojson.Basic.t) list;
}