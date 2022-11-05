(*
 * Converts an FSM into a tabulation fluent API AST.
 *)
structure Fsm2Tabulation = struct
open Util
open MathUtil

(*
 * Fluent API AST.
 *)
datatype ast = Ast of
	(Fsm.letter, int) Map.map (* API functions enumeration *)
  * int option list list (* fluent API signatures *)
  * int (* initial ID *)
  * int Set.set (* accepting IDs *)

(*
 * Builds a complete fluent API AST from an FSM.
 *)
fun ast_build fsm = let
		val letters_ = Fsm.alphabet fsm
		val states_ = Fsm.states fsm
		val m = Set.size letters_
		val n = Set.size states_
		val letters = ListPair.zipEq (letters_, range 0 m)
		val states = ListPair.zipEq (states_, range 0 n)
		val transitions = map (fn q => 
				map (fn l =>
					Option.mapPartial (fn p => Map.get states p) (Fsm.delta fsm q l)
				) letters_
			) states_
		val q0 = valOf (Map.get states (Fsm.initial fsm))
		val F = ((map valOf) o (map (Map.get states))) (Fsm.accepting fsm)
	in
		Ast (letters, transitions, q0, F)
	end

end
