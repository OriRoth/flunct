CM.make "../src/flunct.cm";
open Flunct
val initial = Letter ("initial", ["int"])
val accepting = Letter ("accepting", ["int list"])
val alphabet = Letter ("alphabet", [])
val <-- = Letter ("<--", ["string list"])
val transitions = Letter ("transitions", [])
val >> = Letter (">>", ["int"])
val -- = Letter ("--", ["string"])
val --> = Letter ("-->", ["int"])
val fsm = Fsm (
	State 0,
	[State 5],
	[
		Transition (State 0, initial, State 1),
		Transition (State 1, accepting, State 2),
		Transition (State 2, alphabet, State 3),
		Transition (State 3, --, State 4),
		Transition (State 4, --, State 4),
		Transition (State 4, <--, State 3),
		Transition (State 3, transitions, State 5),
		Transition (State 5, >>, State 6),
		Transition (State 6, --, State 7),
		Transition (State 7, -->, State 5)
	]	
);
print (flunct_api "FSM" fsm Sparse);
