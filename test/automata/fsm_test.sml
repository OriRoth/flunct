structure FsmTest = struct
open Test
open Fsm

(* (aaaa)* *)
val fsm1 = Fsm (
	State 0,
	[State 0],
	[
		Transition (State 0, Letter "a", State 1),
		Transition (State 1, Letter "a", State 2),
		Transition (State 2, Letter "a", State 3),
		Transition (State 3, Letter "a", State 0)
	]
)

(* (abc)* *)
val fsm2 = Fsm (
	State 0,
	[State 0],
	[
		Transition (State 0, Letter "a", State 1),
		Transition (State 1, Letter "b", State 2),
		Transition (State 2, Letter "c", State 0)
	]
)

(* badly designed but legal FSM *)
val fsm3 = Fsm (
	State 0,
	[State 1, State 2],
	[
		Transition (State 3, Letter "a", State 3),
		Transition (State 3, Letter "b", State 4)
	]
)

(* A FSM with two sinks *)
val fsm4 = Fsm (
	State 0,
	[State 1],
	[
		Transition (State 0, Letter "a", State 1),
		Transition (State 0, Letter "b", State 2),
		Transition (State 2, Letter "a", State 3),
		Transition (State 3, Letter "b", State 3)
	]
)

val fsm_test_suit = Suit ("FSM tests", [
	Test ("alphabet", fn () => (let
		val _ = assert "alphabet of fsm1 is [a]"
			(Set.equals [Letter "a"] (alphabet fsm1))
		val _ = assert "alphabet of fsm2 is [a, b, c]"
			(Set.equals [Letter "a", Letter "b", Letter "c"] (alphabet fsm2))
		val _ = assert "alphabet of fsm3 is [a, b]"
			(Set.equals [Letter "a", Letter "b"] (alphabet fsm3))
	in () end)),
	Test ("states", fn () => (let
		val _ = assert "states of fsm1 are [q0, q1, q2, q3]"
			(Set.equals [State 0, State 1, State 2, State 3] (states fsm1))
		val _ = assert "states of fsm2 are [q0, q1, q2]"
			(Set.equals [State 0, State 1, State 2] (states fsm2))
		val _ = assert "states of fsm3 are [q0, q1, q2, q3, q4]"
			(Set.equals [State 0, State 1, State 2, State 3, State 4] (states fsm3))
	in () end)),
	Test ("transitions_of_state", fn () => (let
		val _ = assert "transitions of q0 in fsm1 are [q0-a->q1]"
			(Set.equals [Transition (State 0, Letter "a", State 1)]
				(transitions_of_state fsm1 (State 0)))
		val _ = assert "transitions of q2 in fsm2 are [q2-c->q0]"
			(Set.equals [Transition (State 2, Letter "c", State 0)]
				(transitions_of_state fsm2 (State 2)))
		val _ = assert "transitions of q0 in fsm3 are []"
			(Set.equals []
				(transitions_of_state fsm3 (State 0)))
		val _ = assert "transitions of q3 in fsm3 are [q3-a->q3, q3-b->q4]"
			(Set.equals [Transition (State 3, Letter "a", State 3), Transition (State 3, Letter "b", State 4)]
				(transitions_of_state fsm3 (State 3)))
	in () end)),
	Test ("outgoing_letters", fn () => (let
		val _ = assert "out letters of q0 in fsm1 are [a]"
			(Set.equals [Letter "a"]
				(outgoing_letters fsm1 (State 0)))
		val _ = assert "out letters of q1 in fsm2 are [b]"
			(Set.equals [Letter "b"]
				(outgoing_letters fsm2 (State 1)))
		val _ = assert "out letters of q1 in fsm3 are []"
			(Set.equals []
				(outgoing_letters fsm3 (State 1)))
		val _ = assert "out letters of q3 in fsm3 are [a, b]"
			(Set.equals [Letter "a", Letter "b"]
				(outgoing_letters fsm3 (State 3)))
	in () end)),
	Test ("delta", fn () => (let
		val _ = assert "fsm1: delta(q0, a)=q1" (State 1 = valOf (delta fsm1 (State 0) (Letter "a")))
		val _ = assert "fsm1: delta(q3, a)=q0" (State 0 = valOf (delta fsm1 (State 3) (Letter "a")))
		val _ = assert "fsm2: delta(q2, c)=q0" (State 0 = valOf (delta fsm2 (State 2) (Letter "c")))
		val _ = assert "fsm2: delta(q2, a)=!!" (Option.isNone (delta fsm2 (State 2) (Letter "a")))
	in () end)),
	Test ("acceptable", fn () => (let
		val _ = assert "all states of fsm1 are acceptable"
			(Set.equals (states fsm1) (acceptable fsm1))
		val _ = assert "all states of fsm2 are acceptable"
			(Set.equals (states fsm2) (acceptable fsm2))
		val _ = assert "states q0, q1of fsm4 are acceptable"
			(Set.equals [State 0, State 1] (acceptable fsm4))
	in () end)),
	Test ("is_total", fn () => (let
		val _ = assert "fsm1 is total"
			(is_total fsm1)
		val _ = assert "fsm2 is not total"
			(not (is_total fsm2))
		val _ = assert "fsm3 is not total"
			(not (is_total fsm3))
		val _ = assert "fsm4 is not total"
			(not (is_total fsm4))
	in () end)),
	Test ("make_total", fn () => (let
		val _ = assert "fsm1 is already total"
			(equals fsm1 (make_total fsm1))
		val fsm2_total = make_total fsm2
		val fsm2_sink = hd (Set.subtract (states fsm2_total) (states fsm2))
		val _ = assert "total fsm2 has sink state"
			(4 = (Set.size (states fsm2_total)))
		val _ = assert "total fsm2 goes to sink on [b, c] from q0"
			(Set.equals [Transition (State 0, Letter "a", State 1),
					Transition (State 0, Letter "b", fsm2_sink),
					Transition (State 0, Letter "c", fsm2_sink)]
				(transitions_of_state fsm2_total (State 0)))
		val fsm3_total = make_total fsm3
		val fsm3_sink = hd (Set.subtract (states fsm3_total) (states fsm3))
		val _ = assert "total fsm3 has sink state"
			(6 = (Set.size (states fsm3_total)))
		val _ = assert "total fsm3 goes to sink on [a, b] from q0"
			(Set.equals [Transition (State 0, Letter "a", fsm3_sink),
					Transition (State 0, Letter "b", fsm3_sink)]
				(transitions_of_state fsm3_total (State 0)))
		val _ = assert "total fsm3 does not go to sink from q3"
			(Set.equals [Transition (State 3, Letter "a", State 3),
					Transition (State 3, Letter "b", State 4)]
				(transitions_of_state fsm3_total (State 3)))
	in () end)),
	Test ("add_dummy_states", fn () => (let
		val _ = assert "add 0 dummy states does nothing"
			(equals fsm1 (add_dummy_states fsm1 0))
		val dummy_fsm2 = add_dummy_states fsm2 1
		val _ = assert "adding 1 dummy state: sanity check"
			(Set.size (states dummy_fsm2) = 4)
		val _ = assert "adding 1 dummy state: original transitions do not change"
			(Set.size (transitions dummy_fsm2) = 6)
		val dummy_state2 = hd (Set.subtract (states dummy_fsm2) (states fsm2))
		val _ = assert "adding 1 dummy state: dummy loops to self"
			(Set.equals [(Transition (dummy_state2, Letter "a", dummy_state2)),
				(Transition (dummy_state2, Letter "b", dummy_state2)),
				(Transition (dummy_state2, Letter "c", dummy_state2))]
				(transitions_of_state dummy_fsm2 dummy_state2))
		val dummy_fsm1 = add_dummy_states fsm1 6
		val _ = assert "adding 6 dummy states: sanity check"
			(Set.size (states dummy_fsm1) = 10)
		val _ = assert "adding 6 dummy states: original transitions do not change"
			(Set.size (transitions dummy_fsm1) = 10)
		val dummy_states1 = Set.subtract (states dummy_fsm1) (states fsm1)
		val _ = assert "adding 6 dummy states: dummies loop to themselves"
			(List.all id (map (fn s => (Set.equals [(Transition (s, Letter "a", s))]
				(transitions_of_state dummy_fsm1 s)))
				dummy_states1))
	in () end)),
	Test ("is_canonical", fn () => (let
		val _ = assert "fsm1 is canonical" (is_canonical fsm1)
		val _ = assert "fsm2 is canonical" ((not o is_canonical) fsm2)
		val _ = assert "fsm3 is canonical" ((not o is_canonical) fsm3)
		val _ = assert "fsm4 is canonical" ((not o is_canonical) fsm4)
	in () end)),
	Test ("make_canonical", fn () => (let
		val fsm1_canonical = make_canonical fsm1
		val fsm2_canonical = make_canonical fsm2
		val fsm3_canonical = make_canonical fsm3
		val fsm4_canonical = make_canonical fsm4
		val _ = assert "fsm1 stays the same under canonical transformation" (equals fsm1 fsm1_canonical)
		val _ = assert "canonical fsm2 has 4 states" (4 = (length o states) fsm2_canonical)
		val _ = assert "canonical fsm3 has 8 states" (8 = (length o states) fsm3_canonical)
		val _ = assert "canonical fsm4 has 8 states" (8 = (length o states) fsm4_canonical)
	in () end))
])

end
