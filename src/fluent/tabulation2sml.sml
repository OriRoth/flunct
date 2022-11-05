(*
 * Converts an AST to a tabulation SML fluent API.
 *)
structure Tabulation2Sml = struct
open MathUtil
open Fsm2Tabulation

val sml_start_name = "^^"
val sml_end_name = "$$"
val sml_id_field = "id"
val sml_calls_type_name = "call"
val sml_calls_field = "calls"

(*
 * Prints type $ denoting FSM acceptance.
 *)
val sml_acceptance_token = "datatype $ = $"

(*
 * Prints the indexing functions.
 *)
fun sml_indexing_functions (Ast (_, api, _, _)) = let
		val n = length api
		val parameters = String.concatWith ", " (map (fn i => "x" ^ (Int.toString i)) (range 0 n))
	in
		String.concatWith "\n"
		(map (fn i =>
			"fun I" ^
			(Int.toString i) ^
			" (" ^
			parameters ^
			") = x" ^ (Int.toString i)
		) (range 0 n))
	end

(*
 * Prints the fluent API IDs that encode FSM states.
 *)
fun sml_ids (Ast (funcs, api, _, accepting)) = let
		val n = length api
		val m = Map.size funcs
	in
		(String.concatWith "\n"
		(map (fn (q, delta_q) => 
			"val e" ^
			(Int.toString q) ^
			" = (" ^
			(if Set.contains accepting q then "$" else "()") ^
			", " ^
			(
				String.concatWith ", "
				(map (fn t =>
					case t of
						NONE => "()"
					  | SOME t' => ("I" ^ (Int.toString t'))
				) delta_q)
			) ^
			")"
		) (ListPair.zipEq (range 0 n, api)))) ^
		"\n" ^
		"val e = (" ^
		(String.concatWith ", " (map (fn i => "e" ^ (Int.toString i)) (range 0 n))) ^
		")"
	end

(*
 * Prints the fluent API argument wrappers.
 *)
fun sml_calls (Ast (funcs, _, _, _)) parameters =
	Shuffle2Sml.sml_calls__ (Map.keys funcs) parameters

(*
 * Prints the fluent API initial variable.
 *)
fun sml_start (Ast (_, _, initial_id, _)) parameters =
	"val " ^
	sml_start_name ^
	" = fn f => f { " ^
	sml_id_field ^
	" = e" ^
	(Int.toString initial_id) ^
	", " ^
	sml_calls_field ^
	" = [] : " ^
	(Shuffle2Sml.sml_calls_type parameters) ^
	" list }"

(*
 * Prints the fluent API functions.
 *)
fun sml_api (Ast (funcs, _, _, _)) parameters = let
		val m = length funcs
		fun get_parameters letter = valOf (Map.get parameters letter)
		fun parameter_names letter ln = map (fn i => "x" ^ (Int.toString i)) (range 0 ln)
		fun sml_api_func (Fsm.Letter letter) index = let
				val ln = (length o get_parameters) letter
			in
				"fun " ^
				(lowercase letter) ^
				" { " ^
				sml_id_field ^
				" = (_, " ^
				(String.concatWith ", "
					(map (fn i =>
						if i = index then "I" else "_"
					) (range 0 m))
				) ^
				"), " ^
				sml_calls_field ^
				" : " ^
				(Shuffle2Sml.sml_calls_type parameters) ^
				" list } =" ^
				(if ln = 0 then "" else " fn " ^ (String.concatWith " => fn " (parameter_names letter ln)) ^ " =>") ^
				" fn f => f { " ^
				sml_id_field ^
				" = I e, " ^
				sml_calls_field ^
				" = " ^
				(if ln = 0 then Shuffle2Sml.sml_datatype_value_name letter
					else "(" ^
						(Shuffle2Sml.sml_datatype_value_name letter) ^
						" (" ^
						String.concatWith ", " (parameter_names letter ln) ^
						"))"
				) ^
				"::" ^
				sml_calls_field ^
				" }"
			end
	in
		String.concatWith "\n"
		(map (fn (l, i) =>
			sml_api_func l i
		) funcs)
	end

(*
 * Prints the fluent API termination function.
 * It accepts only IDs that encode accepting FSM states.
 *)
fun sml_end (Ast (funcs, _, _, _)) parameters = let
		val m = length funcs
	in
		"fun " ^
		sml_end_name ^
		" { " ^
		sml_id_field ^
		" = ($, " ^
		(String.concatWith ", " (map (fn _ => "_") (range 0 m))) ^
		"), " ^
		sml_calls_field ^
		" : " ^
		(Shuffle2Sml.sml_calls_type parameters) ^
		" list } = rev " ^
		sml_calls_field
	end

(*
 * Prints the entire fluent API.
 * The fluent API is contained in a structure named api_name.
 * parameters specified the fluent API's function parameters.
 *)
fun sml_fluent_api api_name ast parameters =
	"structure " ^
	api_name ^
	" = struct\n" ^
	"\n" ^
	sml_acceptance_token ^
	"\n" ^
	(sml_indexing_functions ast) ^
	"\n" ^
	(sml_ids ast) ^
	"\n" ^
	(sml_calls ast parameters) ^
	"\n" ^
	(sml_start ast parameters) ^
	"\n" ^
	(sml_api ast parameters) ^
	"\n" ^
	(sml_end ast parameters) ^
	"\n" ^
	"\n" ^
	"end\n"

end
