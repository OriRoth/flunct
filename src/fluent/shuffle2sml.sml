(*
 * Converts an AST to a shuffling SML fluent API.
 * The fluent API uses a generic ID type with 2^|FSM| parameters
 * to enforce the API protocol encoded by the original FSM at compile time.
 * The ID contains the evaluations of every possible binary function on
 * the bits encofing the current FSM state.
 *)
structure Shuffle2Sml = struct
open Util
open Binary2Shuffle

val sml_start_name = "^^"
val sml_end_name = "$$"
val sml_id_type = "id"
val sml_id_value = "ID"
val sml_id_field = "id"
val sml_calls_type_name = "call"
val sml_calls_field = "calls"
val sml_controls_field = "controls"

(*
 * Truth and false type constants.
 *)
val sml_bundled =
	"datatype t = T\n" ^
	"datatype f = F"

(*
 * Prints the type parameter representation of a binary vector.
 *)
fun sml_type binary_function = let
		fun int_ ([], _, result) = result
		  | int_ (Binary.B0::rest, i, result) = int_ (rest, i * 2, result)
		  | int_ (Binary.B1::rest, i, result) = int_ (rest, i * 2, result + i)
		fun int l i = int_ (l, Int.toLarge i, Int.toLarge 0)
	in
		"'f" ^ (LargeInt.toString (int (rev binary_function) 1))
	end

(*
 * Prints the fluent API argument wrappers.
 *)
fun sml_parameters parameters fixed_arguments =
	"(" ^
	(String.concatWith ", " (map (fn (parameter, fixed_argument) =>
		case fixed_argument of
			SOME Binary.B0 => "f"
		  | SOME Binary.B1 => "t"
		  | NONE => sml_type parameter)
		(ListPair.zipEq (parameters, fixed_arguments)))) ^
	")"

(*
 * Prints the fluent API instantaneous description.
 *)
fun sml_id (Ast (parameters, _, _, _, _)) =
	"datatype " ^
	(sml_parameters parameters (map (fn _ => NONE) (range 0 (length parameters)))) ^
	" " ^
	sml_id_type ^
	" = " ^
	sml_id_value

(*
 * Prints a function's parameter types.
 *)
fun sml_calls_type parameters = let
		val type_parameters = (String.concatWith ", "
			o List.filter (fn p => String.sub(p, 0) = #"'")
			o reduce [] Set.insert_all
			o Map.values)
			parameters
	in
		if type_parameters = "" then sml_calls_type_name
		else "(" ^ type_parameters ^ ") " ^ sml_calls_type_name
	end

fun sml_datatype_value_name s =
	if (Char.isAlpha o hd o explode) s then first_upper s else "@" ^ s

fun sml_calls__ alphabet parameters = let
		fun sml_call (Fsm.Letter letter) = let
				val function_parameters = valOf (Map.get parameters letter)
			in
				(sml_datatype_value_name letter) ^
				(if function_parameters = [] then "" else				
					" of " ^
					(String.concatWith " * " function_parameters)
				)
			end
		val letters = alphabet
	in
		"datatype " ^
		(sml_calls_type parameters) ^
		" = " ^ (
			(String.concatWith " | " o map sml_call) letters
		)
	end

(*
 * Prints the fluent API argument wrappers.
 *)
fun sml_calls (Ast (_, _, api, _, _)) parameters =
	sml_calls__ (Map.keys api) parameters

(*
 * Prints the fluent API functions.
 *)
fun sml_api (Ast (type_parameters, _, api, _, debug_arguments)) parameters additional_controls = let
			fun get_parameters letter = valOf (Map.get parameters letter)
			fun parameter_names letter n = map (fn i => "x" ^ (Int.toString i)) (range 0 n)
			fun get_additional_control_in name = default [] ((Option.map (fn (a, _) => a)) (Map.get additional_controls name))
			fun get_additional_control_out name = default [] ((Option.map (fn (_, b) => b)) (Map.get additional_controls name))
			fun sml_function (Fsm.Letter letter, return_parameters) = let
				val n = (length o get_parameters) letter
				fun get_fixed_arguments parameters fixed_arguments = map #1
					(List.filter (fn (_, fixed_argument) => case fixed_argument of
						SOME _ => true
					  | NONE => false)
					(ListPair.zipEq (parameters, fixed_arguments)))
				fun fixed_return_arguments parameters fixed_arguments = map
					(fn parameter => if contains fixed_arguments parameter then SOME Binary.B1 else NONE)
					parameters
				val fixed_arguments_1 = fixed_return_arguments type_parameters
					(get_fixed_arguments return_parameters debug_arguments)
				val fixed_arguments_2 = fixed_return_arguments return_parameters
					(get_fixed_arguments type_parameters fixed_arguments_1)
				val additional_controls_in = get_additional_control_in letter
				val additional_controls_out = get_additional_control_out letter
			in
				"fun " ^
				(lowercase letter) ^
				" { " ^
				sml_id_field ^
				" : " ^
				(sml_parameters type_parameters fixed_arguments_1) ^
				" " ^
				sml_id_type ^
				", " ^
				sml_controls_field ^
				" = (" ^
				(String.concatWith ", " additional_controls_in) ^
				"), " ^
				sml_calls_field ^
				" : " ^
				(sml_calls_type parameters) ^
				" list } =" ^
				(if n = 0 then "" else " fn " ^ (String.concatWith " => fn " (parameter_names letter n)) ^ " =>") ^
				" fn f => f { " ^
				sml_id_field ^
				" = " ^
				sml_id_value ^
				" : " ^
				(sml_parameters return_parameters fixed_arguments_2) ^
				" " ^
				sml_id_type ^
				", " ^
				sml_controls_field ^
				" = (" ^
				(String.concatWith ", " additional_controls_out) ^
				"), " ^
				sml_calls_field ^
				" = " ^
				(if n = 0 then ((sml_datatype_value_name letter) ^ " ")
					else "(" ^
						(sml_datatype_value_name letter) ^
						" (" ^
						String.concatWith ", " (parameter_names letter n) ^
						"))"
				) ^
				"::" ^
				sml_calls_field ^
				" }"
			end
	in
		(String.concatWith "\n" o map sml_function) api
	end

(*
 * Prints the fluent API initial variable.
 *)
fun sml_start (Ast (_, start_arguments, _, _, _)) parameters additional_control_initial = let
		val decoded_start_arguments =
			String.concatWith ", " (map (fn b =>
				case b of
					Binary.B0 => "f"
				  | Binary.B1 => "t"
			) start_arguments)
	in
		"val " ^
		sml_start_name ^
		" = fn f => f { " ^
		sml_id_field ^
		" = " ^
		sml_id_value ^
		" : (" ^
		decoded_start_arguments ^
		") " ^
		sml_id_type ^
		", " ^
		sml_controls_field ^
		" = (" ^
		(String.concatWith ", " additional_control_initial) ^
		"), " ^
		sml_calls_field ^
		" = [] : " ^
		(sml_calls_type parameters) ^
		" list }"
	end

(*
 * Prints the fluent API termination function.
 * It accepts only IDs which encode accepting FSM states.
 *)
fun sml_end (Ast (_, _, _, end_arguments, _)) parameters additional_control_final = let
		val decoded_end_type = String.concatWith ", " (map (fn (b, i) =>
				case b of
					NONE => "'p" ^ (Int.toString i)
				  | SOME _ => "t"
			) (ListPair.zipEq (end_arguments, (range 1 ((length end_arguments) + 1)))))
	in
		"fun " ^
		sml_end_name ^
		" { " ^
		sml_id_field ^
		" : (" ^
		decoded_end_type ^
		") " ^
		sml_id_type ^
		", " ^
		sml_controls_field ^
		" = (" ^
		(String.concatWith ", " additional_control_final) ^
		"), " ^
		sml_calls_field ^
		" : " ^
		(sml_calls_type parameters) ^
		" list } = rev " ^
		sml_calls_field
	end

(*
 * Prints the entire fluent API.
 * The fluent API is contained in a structure named api_name.
 * parameters specified the fluent API's function parameters.
 *)
fun sml_fluent_api api_name ast parameters
	{additional_control_initial, additional_control_final} additional_controls =
	"structure " ^
	api_name ^
	" = struct\n" ^
	"\n" ^
	sml_bundled ^
	"\n" ^
	(sml_calls ast parameters) ^
	"\n" ^
	(sml_id ast) ^
	"\n" ^
	(sml_api ast parameters additional_controls) ^
	"\n" ^
	(sml_start ast parameters additional_control_initial) ^
	"\n" ^
	(sml_end ast parameters additional_control_final) ^
	"\n" ^
	"\n" ^
	"end\n"

end
