(*
 * Operations for the set data structure.
 *)
structure Set = struct
open Util

(*
 * A set is just a list.
 *)
type 'a set = 'a list

(*
 * Checks whether a set contains a given item.
 *
 * contains [1, 2, 3] 2 = true
 *)
val contains = contains

(*
 * Returns the size of the set.
 *
 * size [1, 2, 3] = 3
 *)
val size = length

(*
 * Inserts an item into a set.
 *
 * insert [1, 2, 3] 4 = [1, 2, 3, 4]
 *)
fun insert set item =
	if contains set item then set
	else set @ [item] (* item::set *) (* less efficient, but keeps ordering *)

(*
 * Inserts multiple items into a set.
 *
 * insert_all [1, 2, 3] [2, 3, 4] = [1, 2, 3, 4]
 *)
fun insert_all set items =
	case items of
		[] => set
	  | item::rest => insert_all (insert set item) rest

(*
 * Checks whether the first set contains all the items of the second set.
 *
 * contains_all [1, 2, 3] [2, 3] = true
 *)
fun contains_all set1 set2 =
	case set2 of
		[] => true
	  | item::rest => contains set1 item andalso contains_all set1 rest

(*
 * Returns whether the two sets are equivalent.
 *
 * equals [1, 2, 3] [3, 2, 1] = true
 *)
fun equals set1 set2 = size set1 = size set2 andalso contains_all set1 set2

(*
 * Turns list into a set.
 *
 * Set.make [1, 1, 2, 2, 3] = [1, 2, 3]
 *)
fun make list = reduce [] insert list

(*
 * Subtracts the second set from the first set.
 *
 * subtract [1, 2, 3] [2, 3, 4] = [1]
 *)
fun subtract set1 set2 = remove (contains set2) set1

(*
 * Checks whether two sets are disjoint.
 *
 * disjoint [1, 2, 3] [2, 3, 4] = false
 * disjoint [1, 2, 3] [4, 5, 6] = trues
 *)
fun disjoint set1 set2 = (not o Option.isSome) (List.find (contains set2) set1)

end
