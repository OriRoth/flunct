(*
 * Various utility functions.
 *)
structure Util = struct

(*
 * The identity function.
 *)
fun id x = x

(*
 * Checks whether a list contains a given item.
 *
 * contains [1, 2, 3, 4] 3 = true
 *)
fun contains [] _ = false
  | contains (head::rest) item = item = head orelse contains rest item

(*
 * Reduces list by binary function starting with initial value.
 *
 * reduce 0 plus [1, 2, 3, 4] = 10
 *)
fun reduce initial function list =
	case list of
		[] => initial
	  | head::rest => reduce (function initial head) function rest

(*
 * The opposite of filter.
 *
 * remove (fn n => n<0) [1, ~2, 3, ~4] = [1, 3]
 *)
fun remove predicate list =
	List.filter (fn item => not (predicate item)) list

(*
 * andalso as standard function.
 *)
fun andalso_ x y = x andalso y

(*
 * Returns list maximum.
 *
 * max [1, 3, 2] = 3
 *)
fun max list =
	case list of
		[] => raise Empty
	  | item::rest => reduce item (fn x => fn y => if x >= y then x else y) rest

(*
 * Returns a list of integers in the given range.
 * from is inclusive, to is exclusive.
 *
 * range 0 4 = [0, 1, 2, 3]
 *)
fun range from to =
	if from >= to then []
	else from::(range (from + 1) to)

(*
 * Repeats string n times.
 *
 * repeat 5 "0" = "00000"
 *)
fun repeat n string = String.concat (map (fn _ => string) (range 0 n))

(*
 * Repeats list n times.
 *
 * repeat_list 3 [1, 2] = [1, 2, 1, 2, 1, 2]
 *)
fun repeat_list n list = List.concat (map (fn _ => list) (range 0 n))

(*
 * Returns heads of input lists.
 *
 * heads [[1, 2], [3, 4], [5, 6]] = [1, 3, 5]
 *)
fun heads [] = []
  | heads ((item::_)::rest) = item::(heads rest)
  | heads _ = raise Domain

(*
 * Returns tails of input lists.
 *
 * tails [[1, 2, 3], [4, 5, 6]] = [[2, 3], [5, 6]]
 *)
fun tails [] = []
  | tails ((_::rest1)::rest2) = rest1::(tails rest2)
  | tails _ = raise Domain

(*
 * Transposes a 2D matrix.
 *
 * transpose [[1, 2], [3, 4]] = [[1, 3], [2, 4]]
 *)
fun transpose [] = []
  | transpose lists =
		if List.all (fn l => l = []) lists then []
		else (heads lists)::(transpose (tails lists))

(*
 * Get optional value or default value for NONE.
 *)
fun default value NONE = value
  | default _ (SOME value) = value

(*
 * Makes string uppercase.
 *)
val uppercase = implode o map Char.toUpper o explode

(*
 * Makes string lowercase.
 *)
val lowercase = implode o map Char.toLower o explode

(*
 * Makes the first letter of the string uppercase.
 *)
val first_upper = let
		fun up_first [] = []
		  | up_first (head::rest) = (Char.toUpper head)::rest
	in
		implode o up_first o explode
	end

(*
 * Colors text red.
 *)
fun text_red text = "\027[1;31m" ^ text ^ "\027[0m"

(*
 * Colors text green.
 *)
fun text_green text = "\027[1;32m" ^ text ^ "\027[0m"

(*
 * Colors text background yellow.
 *)
fun text_background_yellow text = "\027[43m" ^ text ^ "\027[0m"

(*
 * Colors text background magenta.
 *)
fun text_background_magenta text = "\027[45m" ^ text ^ "\027[0m"

end
