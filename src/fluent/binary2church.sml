(*
 * Converts a binary-encoded FSM into a Church-encoded fluent API AST.
 *)
structure Binary2Church = struct
open Util
open MathUtil

(*
 * Fluent API AST.
 *)
datatype ast = Ast of
	Binary.bit list (* initial type *)
  * (Fsm.letter, Binary.bit list list list) Map.map (* API functions *)
  * Binary.bit list list (* accepting function *)

(*
 * Builds the fluent API initial type.
 * The initial type directly encodes the initial FSM state.
 *)
fun ast_initial_type (Fsm.Fsm (initial_state, _, _)) states_encoding =
	valOf (Map.get states_encoding initial_state)

(*
 * Builds the fluent API accepting function.
 * This function return true only for binary vectors encoding accepting states.
 *)
fun ast_accepting_type (Fsm.Fsm (_, accepting_states, _)) states_encoding =
	map (valOf o (Map.get states_encoding)) accepting_states

(*
 * Builds the fluent API functions.
 * Each function is a vector of binary functions, applied bitwise to the ID type.
 *)
fun ast_api fsm transitions_encoding = let
		val n = log2 (Set.size (Fsm.states fsm))
		fun fun2dnf [Binary.B0] 0 _ = []
		  | fun2dnf [Binary.B1] 0 conjunction = [rev conjunction]
		  | fun2dnf f n conjunction = let
					val half = pow2 (n - 1)
				in
					(fun2dnf (List.take (f, half)) (n - 1) (Binary.B0::conjunction)) @
					(fun2dnf (List.drop (f, half)) (n - 1) (Binary.B1::conjunction))
				end 
	in
		map (fn letter => (letter,
			map (fn f =>
				fun2dnf f n []
			) (valOf (Map.get transitions_encoding letter))
		)) (Fsm.alphabet fsm)
	end

(*
 * Builds a complete fluent API AST from binary FSM encoding.
 *)
fun ast_build fsm states_encoding transitions_encoding =
	Ast (
		ast_initial_type fsm states_encoding,
		ast_api fsm transitions_encoding,
		ast_accepting_type fsm states_encoding)

end
