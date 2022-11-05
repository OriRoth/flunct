(*
 * The Flunct front-end.
 *)
structure Flunct = struct

(*
 * An FSM state.
 *)
datatype state =
	State of
		int (* state identifier *)

(*
 * An FSM input letter.
 *)
datatype letter =
	Letter of
		string      (* function name *)
	  * string list (* function parameters *)

(*
 * An FSM input letter with additional control.
 *)
datatype letter' =
	Letter' of
		string      (* function name *)
	  * string list (* function parameters *)
	  * string list (* additional control in *)
	  * string list (* additional control out *)

(*
 * An FSM transition.
 *)
datatype transition =
	Transition of
		state  (* source state *)
	  * letter (* input letter *)
	  * state  (* target state *)

(*
 * An FSM transition with additional control.
 *)
datatype transition' =
	Transition' of
		state   (* source state *)
	  * letter' (* input letter *)
	  * state   (* target state *)

(*
 * A deterministic finite state machine with partial transition function.
 *)
datatype fsm =
	Fsm of
		state           (* the initial state *)
	  * state list      (* the set of accepting states *)
	  * transition list (* partial transition function *)

(*
 * A deterministic finite state machine with partial transition function and additional control.
 *)
datatype fsm' =
	Fsm' of
		state            (* the initial state *)
	  * state list       (* the set of accepting states *)
	  * transition' list (* partial transition function *)

val no_additional_controls = {
	additional_control_initial = []
  , additional_control_final = []
}

(*
 * Raised when encountering a problem with the FSM structure.
 *)
exception MalformedFsm of string

(*
 * Supported encoding methods.
 *)
datatype method = Shuffle | Church | Sparse | Tabulation

(*
 * Converts an FSM to an SML fluent API.
 *)
fun flunct_api api_name fsm method = let
		fun flunct_fsm2standard_fsm (Fsm (State initial, accepting, transitions)) =
			Fsm.Fsm (
				Fsm.State initial,
				map (fn (State q) => Fsm.State q) accepting,
				map (fn (Transition (State q, Letter (l, _), State p)) =>
					Fsm.Transition (Fsm.State q, Fsm.Letter l, Fsm.State p)) transitions
			)
		val standard_fsm = flunct_fsm2standard_fsm fsm
		val canonical_fsm = Fsm.make_canonical standard_fsm
		val (states_encoding, transitions_encoding) = Fsm2Binary.fsm_binary_encoding canonical_fsm
		fun get_transitions (Fsm (_, _, transitions)) = transitions
		val transitions = get_transitions fsm
		val parameters = Set.make (
			map (fn (Transition (_, Letter (name, parameters), _)) => (name, parameters))
			transitions
		)
		fun to_string list =
			"[" ^
			(String.concatWith ", " list) ^
			"]"
		(* input validation *)
		fun validate_no_overloading [] _ = ()
		  | validate_no_overloading ((name, parameters)::rest) seen =
			case (Map.get seen name) of
				NONE => validate_no_overloading rest ((name, parameters)::seen)
			  | SOME other_parameters => raise MalformedFsm ("function " ^ name ^
					" is defined twice, with parameters " ^ (to_string other_parameters) ^
					" and " ^ (to_string parameters))
		fun validate_deterministic [] _ = ()
		  | validate_deterministic ((Transition (State source, Letter (name, _), State target))::rest) transitions_mapping =
				case Map.get transitions_mapping (source, name) of
					SOME other_target => if target <> other_target then
							raise MalformedFsm ("FSM is non-deterministic. state " ^ (Int.toString source) ^
								" goes with letter " ^ name ^ " to both states " ^ (Int.toString other_target) ^
								" and " ^ (Int.toString target))
						else validate_deterministic rest transitions_mapping
				  | NONE => validate_deterministic rest (Map.put transitions_mapping (source, name) target)
		val _ = validate_deterministic transitions []
	in
		if method = Shuffle then
			let
				val ast = Binary2Shuffle.ast_build canonical_fsm states_encoding transitions_encoding
			in
				Shuffle2Sml.sml_fluent_api api_name ast parameters no_additional_controls []
			end
		else if method = Church then
			let
				val ast = Binary2Church.ast_build canonical_fsm states_encoding transitions_encoding
			in
				Church2Sml.sml_fluent_api api_name ast parameters
			end
		else if method = Sparse then
			let
				val ast = Binary2Sparse.ast_build canonical_fsm states_encoding transitions_encoding
			in
				Shuffle2Sml.sml_fluent_api api_name ast parameters no_additional_controls []
			end
		else
			let
				val ast = Fsm2Tabulation.ast_build standard_fsm
			in
				Tabulation2Sml.sml_fluent_api api_name ast parameters
			end
		end

(*
 * Converts an FSM (with additional control) to an SML fluent API.
 *)
fun flunct_api' api_name fsm method additional_control_scheme = let
		fun flunct_fsm2standard_fsm (Fsm' (State initial, accepting, transitions)) =
			Fsm.Fsm (
				Fsm.State initial,
				map (fn (State q) => Fsm.State q) accepting,
				map (fn (Transition' (State q, Letter' (l, _, _, _), State p)) =>
					Fsm.Transition (Fsm.State q, Fsm.Letter l, Fsm.State p)) transitions
			)
		val canonical_fsm = Fsm.make_canonical (flunct_fsm2standard_fsm fsm)
		val (states_encoding, transitions_encoding) = Fsm2Binary.fsm_binary_encoding canonical_fsm
		fun get_transitions (Fsm' (_, _, transitions)) = transitions
		val transitions = get_transitions fsm
		val parameters = Set.make (
			map (fn (Transition' (_, Letter' (name, parameters, _, _), _)) => (name, parameters))
			transitions
		)
		val additional_controls = Set.make (
			map (fn (Transition' (_, Letter' (name, _, additional_control_in, additional_control_out), _)) =>
				(name, (additional_control_in, additional_control_out))) transitions
		)
		fun to_string list =
			"[" ^
			(String.concatWith ", " list) ^
			"]"
		(* input validation *)
		fun validate_no_overloading [] _ = ()
		  | validate_no_overloading ((name, parameters)::rest) seen =
			case (Map.get seen name) of
				NONE => validate_no_overloading rest ((name, parameters)::seen)
			  | SOME other_parameters => raise MalformedFsm ("function " ^ name ^
					" is defined twice, with parameters " ^ (to_string other_parameters) ^
					" and " ^ (to_string parameters))
		val _ = validate_no_overloading parameters []
		fun validate_no_control_overloading [] _ = ()
		  | validate_no_control_overloading ((name, additional_control)::rest) seen =
			case (Map.get seen name) of
				NONE => validate_no_control_overloading rest ((name, additional_control)::seen)
			  | SOME other_control => let
						val (in1, out1) = other_control
						val (in2, out2) = additional_control
					in
						raise MalformedFsm ("function " ^ name ^
							" is defined twice, with additional control (" ^ (to_string in1) ^
							", " ^ (to_string out1) ^ " and (" ^ (to_string in2) ^ ", " ^ (to_string out2) ^ ")")
					end
		val _ = validate_no_control_overloading additional_controls []
		val _ = if length (#additional_control_initial additional_control_scheme) <>
				length (#additional_control_final additional_control_scheme) then
					raise MalformedFsm "lengths of additional controls (ins and outs) must be equal accross all API functions"
				else ()
		fun validate_constant_length_additional_controls [] = ()
		  | validate_constant_length_additional_controls ((_, (in', out))::rest) = let
					val len_in = length in'
					val len_out = length out
				in
					if len_in <> len_out orelse len_in <> (length (#additional_control_initial additional_control_scheme)) then
						raise MalformedFsm "lengths of additional controls (ins and outs) must be equal accross all API functions"
					else
						validate_constant_length_additional_controls rest
				end
		val _ = validate_constant_length_additional_controls additional_controls
		fun validate_deterministic [] _ = ()
		  | validate_deterministic ((Transition' (State source, Letter' (name, _, _, _), State target))::rest) transitions_mapping =
				case Map.get transitions_mapping (source, name) of
					SOME other_target => if target <> other_target then
							raise MalformedFsm ("FSM is non-deterministic. state " ^ (Int.toString source) ^
								" goes with letter " ^ name ^ " to both states " ^ (Int.toString other_target) ^
								" and " ^ (Int.toString target))
						else validate_deterministic rest transitions_mapping
				  | NONE => validate_deterministic rest (Map.put transitions_mapping (source, name) target)
		val _ = validate_deterministic transitions []
	in
		if method = Shuffle then
			let
				val ast = Binary2Shuffle.ast_build canonical_fsm states_encoding transitions_encoding
			in
				Shuffle2Sml.sml_fluent_api api_name ast parameters additional_control_scheme additional_controls
			end
		else if method = Sparse then
			let
				val ast = Binary2Sparse.ast_build canonical_fsm states_encoding transitions_encoding
			in
				Shuffle2Sml.sml_fluent_api api_name ast parameters additional_control_scheme additional_controls
			end
		else
			raise Domain
		end

end
