(*
 * Map data structure.
 *)
structure Map = struct

(*
 * A map is a list of key-value pairs.
 *)
type ('k, 'v) map = ('k * 'v) list

(*
 * Returns the size of the map.
 *)
val size = length

(*
 * Returns whether the two maps are equivalent.
 *)
val equals = Set.equals

(*
 * Assigns the value to the given key.
 *)
fun put [] key value = [(key, value)]
  | put ((k, v)::rest) key value =
		if k = key then (key, value)::rest
		else (k, v)::(put rest key value)

(*
 * Checks whether the map contains the key.
 *)
fun contains [] key = false
  | contains ((k, v)::rest) key =
		if k = key then true else contains rest key

(*
 * Fetches a value from the map at the given key.
 *)
fun get [] key = NONE
  | get ((k, v)::rest) key =
		if k = key then SOME v
		else get rest key

(*
 * Returns the map key set.
 *)
fun keys [] = []
  | keys ((k, _)::rest) = k::(keys rest)

(*
 * Returns the list of map values.
 *)
fun values [] = []
  | values ((_, v)::rest) = v::(values rest)

end
