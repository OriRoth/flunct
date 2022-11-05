(*
 * Definition and operations of deterministic finite state automata (FSM).
 *)
structure Fsm = struct
open Util
open MathUtil

(*
 * An FSM state.
 *)
datatype state =
	State of
		int (* state identifier *)

(*
 * Returns the state identifier.
 *)
fun state_id (State id) = id

(*
 * An FSM input letter.
 *)
datatype letter =
	Letter of
		string (* letter name *)

(*
 * An FSM transition.
 *)
datatype transition =
	Transition of
		state  (* source state *)
	  * letter (* input letter *)
	  * state  (* target state *)

(*
 * Returns the transition source state.
 *)
fun transition_source (Transition (source, _, _)) = source

(*
 * Returns the transition input letter.
 *)
fun transition_letter (Transition (_, letter, _)) = letter

(*
 * Returns the transition target state.
 *)
fun transition_target (Transition (_, _, target)) = target

(*
 * A deterministic finite state machine with partial transition function.
 *)
datatype fsm =
	Fsm of
		state              (* the initial state *)
	  * state Set.set      (* the set of accepting states *)
	  * transition Set.set (* partial transition function *)

(*
 * Returns the FSM initial state.
 *)
fun initial (Fsm (initial, _, _)) = initial

(*
 * Returns the FSM accepting states.
 *)
fun accepting (Fsm (_, accepting, _)) = accepting

(*
 * Returns the FSM transitions.
 *)
fun transitions (Fsm (_, _, transitions)) = transitions

(*
 * Returns the FSM alphabet.
 *)
fun alphabet fsm =
	Set.make (map transition_letter (transitions fsm))

(*
 * Returns the FSM states.
 * Note that the FSM contains at least a single state.
 *)
fun states fsm =
	Set.insert_all [initial fsm] (
	Set.insert_all (accepting fsm) (
	Set.insert_all (map transition_source (transitions fsm))
		(map transition_target (transitions fsm))))

(*
 * Checks whether the two FSMs are equal.
 *)
fun equals fsm1 fsm2 =
	initial fsm1 = initial fsm1 andalso
	(Set.equals (accepting fsm1) (accepting fsm2)) andalso
	(Set.equals (transitions fsm1) (transitions fsm2))

(*
 * Returns the outgoing transitions of the given state.
 *)
fun transitions_of_state fsm state =
	List.filter (fn t => state = (transition_source t)) (transitions fsm)

(*
 * Returns the letters on the outgoing edges of the given state.
 *)
fun outgoing_letters fsm state =
	map transition_letter (transitions_of_state fsm state)

(*
 * Returns the target state of given source state and input letter.
 *)
fun delta fsm state letter =
	case List.filter (fn t => state = (transition_source t) andalso
			letter = (transition_letter t)) (transitions fsm) of
		[Transition(_, _, target)] => SOME target
	  | _ => NONE

(*
 * Returns the states from which an accepting state is reachable (non-sink)
 *)
fun acceptable fsm = let
		val states = states fsm
		fun get_reaching_states accepting_states =
			List.filter (fn q => if Set.contains accepting_states q then false else let
				val outgoing_transitions = transitions_of_state fsm q
				val target_states = map (fn (Transition (_, _, target)) => target) outgoing_transitions
			in
				not (Set.disjoint target_states accepting_states)
			end) states
		fun get_all_reaching_states accepting_states = let
				val reaching_states = get_reaching_states accepting_states
			in
				if length reaching_states = 0 then accepting_states
				else get_all_reaching_states (Set.insert_all accepting_states reaching_states)
			end
	in
		get_all_reaching_states (accepting fsm)
	end

(*
 * Checks whether the FSM transition function is total.
 *)
fun is_total fsm =
	let
		fun state_total alphabet state =
			Set.equals alphabet (outgoing_letters fsm state)
	in
		List.all (state_total (alphabet fsm)) (states fsm)
	end

(*
 * Returns an equivalent FSM whose transition function is total.
 * May add an auxiliary "sink" state.
 *)
fun make_total fsm =
	let
		fun generate_state_id fsm =
			(max (map state_id (states fsm))) + 1
		fun complete_transitions alphabet sink state =
			map (fn letter => Transition (state, letter, sink))
				(Set.subtract alphabet (outgoing_letters fsm state))
	in
		if is_total fsm then fsm else
		Fsm (initial fsm,
			accepting fsm,
			let
				val sink_state = (State (generate_state_id fsm))
				val transitions_to_sink = List.concat (map
					(complete_transitions (alphabet fsm) sink_state)
					(states fsm))
				val transitions_from_sink_to_sink = map
					(fn l => Transition (sink_state, l, sink_state))
					(alphabet fsm)
			in
				Set.make ((transitions fsm) @ transitions_to_sink @ transitions_from_sink_to_sink)
			end)
	end

(*
 * Adds sink, dummy states to the FSM.
 *)
fun add_dummy_states fsm 0 = fsm
  | add_dummy_states fsm count =
		let
			val first_dummy_id = (max (map state_id (states fsm))) + 1
			val dummy_ids = (range first_dummy_id (first_dummy_id + count))
			val dummy_states = map State dummy_ids
			val alphabet = alphabet fsm
			fun loop_self state = map (fn l => Transition (state, l, state)) alphabet
		in
			Fsm (initial fsm, accepting fsm,
				Set.make ((transitions fsm) @
					(List.concat (map loop_self dummy_states)
			)))
		end

(*
 * Checks whether the FSM is canonical.
 * A canonical FSM has a total transition function and
 * the number of its states is a power of 2.
 *)
fun is_canonical fsm =
	is_pow2 (length (states fsm)) andalso
	is_total fsm

(*
 * Converts an FSM into a canonical FSM.
 * (see Fsm.is_canonical.)
 *)
fun make_canonical fsm =
	if is_canonical fsm then fsm
	else let
		val total_fsm = make_total fsm
		val n = Set.size (states total_fsm)
	in
		if is_pow2 n then total_fsm
		else add_dummy_states total_fsm ((pow2 ((log2 n) + 1)) - n)
	end

end
