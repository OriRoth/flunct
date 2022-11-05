(*
 * Converts a binary-encoded FSM into a bit-shuffling fluent API AST.
 *)
structure Binary2Shuffle = struct
open MathUtil

(*
 * Fluent API abstract syntax tree. Contains the chain
 * start and chain end types and the API function signatures.
 *)
datatype ast = Ast of
	Binary.bit list list                       (* the fluent API generics *)
  * Binary.bit list                            (* fluent chain start type *)
  * (Fsm.letter, Binary.bit list list) Map.map (* fluent API *)
  * Binary.bit option list                     (* fluent chain end type *)
  * Binary.bit option list                     (* fluent chain intermediate evaluation type *)

(*
 * N is the log of the number of FSM states.
 *)
fun N fsm = log2 (Set.size (Fsm.states fsm))

(*
 * Returns the type parameters used in function datatypes.
 * These parameters enumerate the binary functions of n bits.
 *)
fun ast_data_type fsm = Binary.matrices (N fsm)

(*
 * Returns the fluent start type. This type enumerates
 * the results of applying every possible binary function
 * to the binary encoding of the initial state.
 *)
fun ast_initial_type fsm states_encoding funcs = let
		val initial_encoding = valOf (Map.get states_encoding (Fsm.initial fsm))
	in
		map (fn f =>
			Binary.function f initial_encoding
		) funcs
	end

fun ast_accepting_type_ fsm states_encoding funcs accepting_states = let
		fun is_accepting_function f =
			f = (map (fn q =>
				if (Set.contains accepting_states q) then Binary.B1 else Binary.B0
			) (Map.keys states_encoding))
	in
		map (fn f =>
			if is_accepting_function f then SOME Binary.B1 else NONE
		) funcs
	end

(*
 * Returns the fluent accepting type. This type is a binary
 * function that returns 1 if and only if its input encodes
 * an accepting state.
 *)
fun ast_accepting_type fsm states_encoding funcs =
	ast_accepting_type_ fsm states_encoding funcs (Fsm.accepting fsm)

(*
 * Returns the fluent "intermediate evaluation" type. This type is a binary
 * function that returns 1 if and only if its input encodes
 * a state from which an accepting state is reachable.
 *)
fun ast_debug_type fsm states_encoding funcs =
	ast_accepting_type_ fsm states_encoding funcs (Fsm.acceptable fsm)

(*
 * Returns the fluent API function signatures. Each signature
 * comopses all possible binary functions with the binary
 * functions encoding the FSM transitions.
 *)
fun ast_api fsm transitions_encoding funcs =
	map (fn (l, fs) => (l,
		map (fn f =>
			Binary.function_compose f fs
		) funcs
	)) transitions_encoding

fun ast_build_ fsm states_encoding transitions_encoding funcs =
	Ast (funcs,
		 ast_initial_type fsm states_encoding funcs,
		 ast_api fsm transitions_encoding funcs,
		 ast_accepting_type fsm states_encoding funcs,
		 ast_debug_type fsm states_encoding funcs)

(*
 * Build a fluent API AST from FSM binary encoding.
 * (See ast_data_type, ast_initial_type, ast_api, ast_accepting_type.)
 *)
fun ast_build fsm states_encoding transitions_encoding =
	ast_build_ fsm states_encoding transitions_encoding (ast_data_type fsm)

end
