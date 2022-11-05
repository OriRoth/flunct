CM.make "../src/flunct.cm";
open Flunct
(* alphabet *)
val name = Letter ("name", ["string"])
val description = Letter ("description", ["string"])
val optional = Letter ("optional", [])
val argument = Letter ("argument", ["string"])
val alias = Letter ("||", ["string"])
val argtype = Letter ("argtype", ["string"])
(* FSM definition *)
val fsm = Fsm (
	State 0, (* initial state *)
	[State 1, State 2, State 4, State 5], (* accepting states *)
	[ (* FSM transitions *)
		Transition (State 0, name, State 1),
		Transition (State 1, description, State 2),
		Transition (State 1, optional, State 3),
		Transition (State 1, argument, State 4),
		Transition (State 2, optional, State 3),
		Transition (State 2, argument, State 4),
		Transition (State 3, argument, State 4),
		Transition (State 4, description, State 2),
		Transition (State 4, optional, State 3),
		Transition (State 4, alias, State 4),
		Transition (State 4, argument, State 4),
		Transition (State 4, argtype, State 5),
		Transition (State 5, description, State 2),
		Transition (State 5, optional, State 3),
		Transition (State 5, argument, State 4),
		Transition (State 5, alias, State 5)
	]	
);
(* print fluent API *)
print (flunct_api "CLI" fsm Sparse);
