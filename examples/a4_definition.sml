CM.make "../src/flunct.cm";
open Flunct
(* alphabet *)
val a = Letter ("a", [])
(* FSM definition *)
val fsm = Fsm (
	State 0, (* initial state *)
	[State 0], (* accepting states *)
	[ (* FSM transitions *)
		Transition (State 0, a, State 1),
		Transition (State 1, a, State 2),
		Transition (State 2, a, State 3),
		Transition (State 3, a, State 0)
	]	
)
(* generate fluent API *)
val fluent_api = flunct_api "A4" fsm Sparse
(* print fluent API to a4_api.sml *)
val file_out = TextIO.openOut "a4_api.sml"
val _ = TextIO.output (file_out, fluent_api)
val _ = TextIO.flushOut file_out
val _ = TextIO.closeOut file_out
val _ = OS.Process.exit OS.Process.success
