(*
 * Converts a binary-encoded FSM into a sparse bit-shuffling fluent API AST.
 * The encoding is similar to the standard shuffling case (see binary2shuffle.sml),
 * except that functions which never participate in the termination/intermediate evaluations
 * are removed from the ID.
 *)
structure Binary2Sparse = struct
open Util
open MathUtil

(*
 * Builds the sparse instantaneous description.
 *)
fun ast_data_type fsm states_encoding transitions_encoding = let
		val N = Set.size (Fsm.states fsm)
		val accepting_encodings = map (valOf o (Map.get states_encoding)) (Fsm.accepting fsm)
		val accepting_function = reduce (repeat_list N [Binary.B0])
			(fn m => fn I => Binary.matrix_set m I Binary.B1)
			accepting_encodings
		(* acceptable in addition to accepting allows early failures *)
		val acceptable_encodings = map (valOf o (Map.get states_encoding)) (Fsm.acceptable fsm)
		val acceptable_function = reduce (repeat_list N [Binary.B0])
			(fn m => fn I => Binary.matrix_set m I Binary.B1)
			acceptable_encodings
		fun reachable_functions seen [] = seen
		  | reachable_functions seen next = let
		  		val next_next =
		  			Set.subtract (List.concat (map (fn (_, fs) =>
							map (fn f =>
								Binary.function_compose f fs
							) next
						) transitions_encoding)) seen
				in
					reachable_functions (Set.insert_all seen next) next_next
				end
	in
		Set.insert_all (reachable_functions [] [accepting_function]) (reachable_functions [] [acceptable_function])
	end

(*
 * Builds a complete fluent API AST from binary FSM encoding.
 *)
fun ast_build fsm states_encoding transitions_encoding =
	Binary2Shuffle.ast_build_ fsm states_encoding transitions_encoding
	(ast_data_type fsm states_encoding transitions_encoding)

end
