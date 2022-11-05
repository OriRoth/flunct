(*
 * Converts an AST to a Church-based SML fluent API.
 * The fluent API uses a modified Church encoding that works at
 * compile time to enforce the API protocol encoded by the original FSM.
 *)
structure Church2Sml = struct
open MathUtil
open Binary2Church

val sml_start_name = "^^"
val sml_end_name = "$$"
val sml_id_type = "id"
val sml_id_value = "ID"
val sml_id_field = "id"
val sml_calls_type_name = "call"
val sml_calls_field = "calls"

val T = "T"
val F = "F"
val N = "N"
val C = "C"

fun n (Ast (initial_type, _, _)) = length initial_type

(*
 * Church's Boolean encofing and Mairson's pair and copy functions.
 *)
val sml_bundled =
	"fun T x _ = x\n" ^
	"fun F _ y = y\n" ^
	"fun N p = p F T\n" ^
	"fun P x y z = z x y\n" ^
	"fun C p = p (P T T) (P F F)"

(*
 * Prints the fluent API instantaneous description.
 *)
fun sml_id ast =
	"datatype (" ^
	(String.concatWith ", " (map (fn i => "'b" ^ (Int.toString i)) (range 0 (n ast)))) ^
	") " ^
	sml_id_type ^
	" = " ^
	sml_id_value ^
	" of " ^
	(String.concatWith " * " (map (fn i => "'b" ^ (Int.toString i)) (range 0 (n ast))))

(*
 * Prints the fluent API argument wrappers.
 *)
fun sml_calls (Ast (_, api, _)) parameters =
	Shuffle2Sml.sml_calls__ (Map.keys api) parameters

(*
 * Prints the fluent API initial variable.
 *)
fun sml_start (Ast (initial_type, _, _)) parameters = let
		val decoded_start_arguments =
			String.concatWith ", " (map (fn b =>
				case b of
					Binary.B0 => "F"
				  | Binary.B1 => "T"
			) initial_type)
	in
		"val " ^
		sml_start_name ^
		" = fn f => f { " ^
		sml_id_field ^
		" = " ^
		sml_id_value ^
		" (" ^
		decoded_start_arguments ^
		"), " ^
		sml_calls_field ^
		" = [] : " ^
		(Shuffle2Sml.sml_calls_type parameters) ^
		" list }"
	end

fun encode_funcs_0_on n =
	(if n > 1 then "(" else "") ^
	(String.concatWith ", " (map (fn _ => F) (range 0 n))) ^
	(if n > 1 then ")" else "")

fun copies_ i n_on = let
		val I = Int.toString i
		fun copy j =
			"(fn b" ^
			I ^
			"_" ^
			(Int.toString j) ^
			" => fn b" ^
			I ^
			"_" ^
			(Int.toString j) ^
			"_ => (" ^
			C ^
			" b" ^
			I ^
			"_" ^
			(Int.toString j) ^
			"_)"
	in
		"(" ^
		C ^
		" b" ^
		I ^
		") " ^
		(String.concatWith " " (map copy (range 0 (n_on - 2)))) ^
		(if n_on > 2 then " " else "") ^
		"(fn b" ^
		I ^
		"_" ^
		(Int.toString (n_on - 2)) ^
		" => fn b" ^
		I ^
		"_" ^
		(Int.toString (n_on - 1)) ^
		" =>"
	end

fun copies n n_on = (String.concatWith " " (map (fn i => copies_ i n_on) (range 0 n))) ^ " "

fun closes n n_on = repeat ((n_on - 1) * n) ")"

fun encode_bit Binary.B0 i j = "(" ^ N ^ " b" ^ (Int.toString j) ^ "_" ^ (Int.toString i) ^ ")"
  | encode_bit Binary.B1 i j = "b" ^ (Int.toString j) ^ "_" ^ (Int.toString i)

fun encode_conj_ [bit] i j = encode_bit bit i j
  | encode_conj_ (bit::rest) i j = (encode_bit bit i j) ^ " (" ^ (encode_conj_ rest i (j + 1)) ^ ") " ^ F
  | encode_conj_ _ _ _ = raise Domain

fun encode_conj conj i = encode_conj_ conj i 0

fun encode_dnf [] _ = F
  | encode_dnf [conj] i = encode_conj conj i
  | encode_dnf (conj::rest) i = "(" ^ (encode_conj conj i) ^ ") " ^ T ^ " (" ^ (encode_dnf rest (i + 1)) ^ ")"

fun encode_dnfs_ [func] i = encode_dnf func i
  | encode_dnfs_ (func::rest) i = (encode_dnf func i) ^ ", " ^ (encode_dnfs_ rest (i + (length func)))
  | encode_dnfs_ _ _ = raise Domain

fun encode_dnfs funcs =
	(if (length funcs) > 1 then "(" else "") ^
	(encode_dnfs_ funcs 0) ^ 
	(if (length funcs) > 1 then ")" else "")

val encode_funcs_1_on = encode_dnfs

fun encode_funcs_n_on funcs n n_on =
	(copies n n_on) ^
	(encode_dnfs funcs) ^
	(closes n n_on)

fun encode_funcs n funcs = let
		val n_on = ((reduce 0 plus) o (map length)) funcs
	in
		if n_on = 0 then encode_funcs_0_on n else
		if n_on = 1 then encode_funcs_1_on funcs else
		encode_funcs_n_on funcs n n_on
	end
	
(*
 * Prints the fluent API functions.
 * Each function applies the original FSM transition function as a bit function to the ID a la Church encoding.
 *)
fun sml_api (Ast (initial_type, api, _)) parameters = let
			val n = length initial_type
			fun get_parameters letter = valOf (Map.get parameters letter)
			fun parameter_names letter ln = map (fn i => "x" ^ (Int.toString i)) (range 0 ln)
			fun sml_function (Fsm.Letter letter, return_parameters) = let
				val ln = (length o get_parameters) letter
				val n_on = ((reduce 0 plus) o (map length)) return_parameters
			in
				"fun " ^
				(lowercase letter) ^
				" { " ^
				sml_id_field ^
				" = " ^
				sml_id_value ^
				" (" ^
				(String.concatWith ", " (map (fn i => if n_on = 0 then "_" else "b" ^ (Int.toString i) ^ (if n_on = 1 then "_0" else "")) (range 0 n))) ^
				"), " ^
				sml_calls_field ^
				" : " ^
				(Shuffle2Sml.sml_calls_type parameters) ^
				" list } =" ^
				(if ln = 0 then "" else " fn " ^ (String.concatWith " => fn " (parameter_names letter ln)) ^ " =>") ^
				" fn f => f { " ^
				sml_id_field ^
				" = " ^
				sml_id_value ^
				" " ^
				(if n_on > 1 then "(" else "") ^
				(encode_funcs n return_parameters) ^
				(if n_on > 1 then ")" else "") ^
				", " ^
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
		(String.concatWith "\n" o map sml_function) api
	end

(*
 * Prints the fluent API termination function.
 * It accepts only IDs that encode accepting FSM states.
 *)
fun sml_end (Ast (initial_type, _, end_arguments)) parameters = let
		val n = length initial_type
		val n_on = ((reduce 0 plus) o (map length)) [end_arguments]
		val func = encode_funcs n [end_arguments]
	in
		"fun " ^
		sml_end_name ^
		" { " ^
		sml_id_field ^
		" = " ^
		sml_id_value ^
		" (" ^
			(String.concatWith ", "
				(map (fn i => "b" ^ (Int.toString i) ^ (if n_on <= 1 then "_0" else "")) (range 0 n))) ^
		"), " ^
		sml_calls_field ^
		" : " ^
		(Shuffle2Sml.sml_calls_type parameters) ^
		" list } = let val _  = ((" ^
		func ^
		") 0 \"\") + 0 in rev " ^
		sml_calls_field
		^ " end"
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
	sml_bundled ^
	"\n" ^
	(sml_calls ast parameters) ^
	"\n" ^
	(sml_id ast) ^
	"\n" ^
	(sml_api ast parameters) ^
	"\n" ^
	(sml_start ast parameters) ^
	"\n" ^
	(sml_end ast parameters) ^
	"\n" ^
	"\n" ^
	"end\n"

end
