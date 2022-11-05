(*
 * Produces the binary encoding of a FSM, in which states
 * are encoded in binary (q5 => 101) and transitions are
 * encoded as binary functions on these bits.
 *)
structure Fsm2Binary = struct
open Util
open MathUtil

(*
 * Returns a mapping of FSM states to their binary encoding.
 *)
fun fsm_states_binary_encoding fsm = let
		val states = Fsm.states fsm
		val log_num_states = log2 (Set.size states)
		val binaries = Binary.arrays log_num_states
	in
		ListPair.zipEq (states, binaries)
	end

(*
 * Returns a mapping of FSM transition to their binary encoding (matrix).
 * states_binary_encoding is a the *sorted* binary encoding of FSM states.
 * (see states_binary_encoding.)
 *)
fun fsm_transitions_binary_encoding fsm states_binary_encoding = let
		val alphabet = Fsm.alphabet fsm
		fun fsm_transitions_of_letter letter =
			List.filter (fn t => Fsm.transition_letter t = letter) (Fsm.transitions fsm)
		fun fsm_transitions_to_binary_function ts = (* transitions belong to single letter *)
			transpose (map (fn state => let
					val transition = hd (List.filter (fn t => Fsm.transition_source t = state) ts)
				in
					valOf (Map.get states_binary_encoding (Fsm.transition_target transition))
				end
			) (Map.keys states_binary_encoding))
	in
		map (fn l => (l,
			fsm_transitions_to_binary_function (fsm_transitions_of_letter l)
		)) alphabet
	end

(*
 * Returns the binary encoding of the given FSM.
 * (see Fsm.states_binary_encoding, Fsm.transitions_binary_encoding.)
 *)
fun fsm_binary_encoding fsm = let
		val states = fsm_states_binary_encoding fsm
	in
		(states, fsm_transitions_binary_encoding fsm states)
	end

end
