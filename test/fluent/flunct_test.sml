structure FlunctTest = struct
open Test
open Util
open Flunct

(*********
 * Setup *
 *********)

datatype fluent_api_test_case = FluentAPITestCase of
	string           (* fluent API name *)
  * fsm              (* fluent API protocol *)
  * method           (* fluent API encoding method *)
  * string list      (* good compilation chains examples *)
  * string list      (* bad compilation chains examples *)
  * (string->string) (* SML file name -> compilation command *)

fun mlton f = "mlton -stop tc " ^ f ^ " 2> /dev/null"

fun get_test_name fluent_api_name example = fluent_api_name ^ " / " ^ example

fun get_tests (FluentAPITestCase (name, fsm, method, good_examples, bad_examples, compile)) = let
		val fluent_api = flunct_api name fsm method
		fun get_example_test is_good example = Test (get_test_name name example,
			fn () => let
				val program = fluent_api ^ "open " ^ name ^ ";\nval _ = " ^ example ^ ";\n"
				val temp_file_ = OS.FileSys.tmpName ()
				val _ = OS.FileSys.rename {old = temp_file_, new = temp_file_ ^ ".sml"}
				val temp_file = temp_file_ ^ ".sml"
				val file_out = TextIO.openOut temp_file
				val _ = TextIO.output (file_out, program)
				val _ = TextIO.flushOut file_out
				val _ = TextIO.closeOut file_out
				val result = ((if is_good then id else not) o OS.Process.isSuccess) (OS.Process.system (compile temp_file))
				val _ = OS.FileSys.remove temp_file
				val _ = assert (example ^ " is a " ^ (if is_good then "good" else "bad") ^ " chain") result
			in () end)
	in
		(map (get_example_test true) good_examples) @
		(map (get_example_test false) bad_examples)
	end

(********
 * FSMs *
 ********)

val ab_fsm =
	Fsm (
		State 0,
		[State 0],
		[
			Transition (State 0, Letter ("a", []), State 1),
			Transition (State 1, Letter ("b", []), State 0)
		]
	)

val a2_fsm =
	Fsm (
		State 0,
		[State 0, State 1],
		[
			Transition (State 0, Letter ("a", []), State 1),
			Transition (State 1, Letter ("a", []), State 0)
		]
	)

val a3_fsm =
	Fsm (
		State 0,
		[State 0, State 1, State 2],
		[
			Transition (State 0, Letter ("a", []), State 1),
			Transition (State 1, Letter ("a", []), State 2),
			Transition (State 2, Letter ("a", []), State 0)
		]
	)

val a4_fsm =
	Fsm (
		State 0,
		[State 0],
		[
			Transition (State 0, Letter ("a", []), State 1),
			Transition (State 1, Letter ("a", []), State 2),
			Transition (State 2, Letter ("a", []), State 3),
			Transition (State 3, Letter ("a", []), State 0)
		]
	)

val xyz_fsm =
	Fsm (
		State 0,
		[State 0],
		[
			Transition (State 0, Letter ("x", ["'a"]), State 1),
			Transition (State 1, Letter ("y", ["'b", "int"]), State 2),
			Transition (State 2, Letter ("z", []), State 0)
		]
	)

val html_fsm =
	Fsm (
		State 0,
		[State 4],
		[
			Transition (State 0, Letter ("html", []), State 1),
			Transition (State 1, Letter ("body", []), State 2),
			Transition (State 2, Letter ("h1", ["string"]), State 2),
			Transition (State 2, Letter ("p", ["string"]), State 2),
			Transition (State 2, Letter ("a", ["string", "string"]), State 2),
			Transition (State 2, Letter ("img", ["string"]), State 2),
			Transition (State 2, Letter ("table", []), State 5),
			Transition (State 5, Letter ("tr", []), State 6),
			Transition (State 6, Letter ("th", ["string"]), State 6),
			Transition (State 6, Letter ("td", []), State 7),
			Transition (State 7, Letter ("h1", ["string"]), State 7),
			Transition (State 7, Letter ("p", ["string"]), State 7),
			Transition (State 7, Letter ("a", ["string", "string"]), State 7),
			Transition (State 7, Letter ("img", ["string"]), State 7),
			Transition (State 7, Letter ("done", []), State 6),
			Transition (State 6, Letter ("done", []), State 5),
			Transition (State 5, Letter ("done", []), State 2),
			Transition (State 2, Letter ("done", []), State 3),
			Transition (State 3, Letter ("done", []), State 4)
		]
	)

(**************
 * Test Cases *
 **************)

val test_cases = [
	FluentAPITestCase (
		"A2_Church",
		a2_fsm,
		Church,
		["^^ $$", "^^ a $$", "^^ a a $$", "^^ a a a $$", "^^ a a a a $$"],
		[],
		mlton
	),
	FluentAPITestCase (
		"A2_Shuffle",
		a2_fsm,
		Shuffle,
		["^^ $$", "^^ a $$", "^^ a a $$", "^^ a a a $$", "^^ a a a a $$"],
		[],
		mlton
	),
	FluentAPITestCase (
		"A2_Sparse",
		a2_fsm,
		Sparse,
		["^^ $$", "^^ a $$", "^^ a a $$", "^^ a a a $$", "^^ a a a a $$"],
		[],
		mlton
	),
	FluentAPITestCase (
		"A2_Tabulation",
		a2_fsm,
		Tabulation,
		["^^ $$", "^^ a $$", "^^ a a $$", "^^ a a a $$", "^^ a a a a $$"],
		[],
		mlton
	),
	FluentAPITestCase (
		"A3_Church",
		a3_fsm,
		Church,
		["^^ $$", "^^ a $$", "^^ a a $$", "^^ a a a $$", "^^ a a a a $$"],
		[],
		mlton
	),
	FluentAPITestCase (
		"A3_Shuffle",
		a3_fsm,
		Shuffle,
		["^^ $$", "^^ a $$", "^^ a a $$", "^^ a a a $$", "^^ a a a a $$"],
		[],
		mlton
	),
	FluentAPITestCase (
		"A3_Sparse",
		a3_fsm,
		Sparse,
		["^^ $$", "^^ a $$", "^^ a a $$", "^^ a a a $$", "^^ a a a a $$"],
		[],
		mlton
	),
	FluentAPITestCase (
		"A3_Tabulation",
		a3_fsm,
		Tabulation,
		["^^ $$", "^^ a $$", "^^ a a $$", "^^ a a a $$", "^^ a a a a $$"],
		[],
		mlton
	),
	FluentAPITestCase (
		"AB_Church",
		ab_fsm,
		Church,
		["^^ $$", "^^ a b $$", "^^ a b a b $$"],
		["^^ a $$", "^^ a a $$", "^^ a b a $$", "^^ a b a b a b a b a b a b a b b $$"],
		mlton
	),
	FluentAPITestCase (
		"AB_Shuffle",
		ab_fsm,
		Shuffle,
		["^^ $$", "^^ a b $$", "^^ a b a b $$", "^^ a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b $$"],
		["^^ a $$", "^^ b", "^^ a a", "^^ a b a $$", "^^ a b a b a b a b a b a b a b a $$",
		 "^^ a b a b a b a b a b a b a b b"],
		mlton
	),
	FluentAPITestCase (
		"AB_Sparse",
		ab_fsm,
		Sparse,
		["^^ $$", "^^ a b $$", "^^ a b a b $$", "^^ a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b $$"],
		["^^ a $$", "^^ b", "^^ a a", "^^ a b a $$", "^^ a b a b a b a b a b a b a b a $$",
		 "^^ a b a b a b a b a b a b a b b"],
		mlton
	),
	FluentAPITestCase (
		"AB_Tabulation",
		ab_fsm,
		Tabulation,
		["^^ $$", "^^ a b $$", "^^ a b a b $$"],
		["^^ a $$", "^^ a a $$", "^^ a b a $$", "^^ a b a b a b a b a b a b a b b $$"],
		mlton
	),
	FluentAPITestCase (
		"A4_Church",
		a4_fsm,
		Church,
		["^^ $$", "^^ a a a a $$"],
		["^^ a $$", "^^ a a $$", "^^ a a a $$", "^^ a a a a a a a a a a a a a $$"],
		mlton
	),
	FluentAPITestCase (
		"A4_Shuffle",
		a4_fsm,
		Shuffle,
		["^^ $$", "^^ a a a a $$", "^^ a a a a a a a a a a a a a a a a a a a a a a a a a a a a $$", "^^"],
		["^^ a $$", "^^ a a $$", "^^ a a a $$", "^^ a a a a a a a a a a a a a $$"],
		mlton
	),
	FluentAPITestCase (
		"A4_Sparse",
		a4_fsm,
		Sparse,
		["^^ $$", "^^ a a a a $$", "^^ a a a a a a a a a a a a a a a a a a a a a a a a a a a a $$", "^^"],
		["^^ a $$", "^^ a a $$", "^^ a a a $$", "^^ a a a a a a a a a a a a a $$"],
		mlton
	),
	FluentAPITestCase (
		"A4_Tabulation",
		a4_fsm,
		Tabulation,
		["^^ $$", "^^ a a a a $$"],
		["^^ a $$", "^^ a a $$", "^^ a a a $$", "^^ a a a a a a a a a a a a a $$"],
		mlton
	),
	FluentAPITestCase (
		"XYZ_Church",
		xyz_fsm,
		Church,
		["^^ $$", "^^ x 0 y 1 2 z $$", "^^ x \"0\" y true 2 z x \"3\" y false 5 z $$"],
		["^^ x 0 $$", "^^ x 0 x 1 y true 3 z $$", "^^ x \"0\" y true 2 x 3",
		 "^^ x \"0\" y true 2 z x 3 y false 5 z $$"],
		mlton
	),
	FluentAPITestCase (
		"XYZ_Shuffle",
		xyz_fsm,
		Shuffle,
		["^^ $$", "^^ x 0 y 1 2 z $$", "^^ x \"0\" y true 2 z x \"3\" y false 5 z $$", "^^", "^^ x 0"],
		["^^ x 0 $$", "^^ x 0 x 1", "^^ x \"0\" y true 2 x 3", "^^ x \"0\" y true 2 z x 3"],
		mlton
	),
	FluentAPITestCase (
		"XYZ_Sparse",
		xyz_fsm,
		Sparse,
		["^^ $$", "^^ x 0 y 1 2 z $$", "^^ x \"0\" y true 2 z x \"3\" y false 5 z $$", "^^", "^^ x 0"],
		["^^ x 0 $$", "^^ x 0 x 1", "^^ x \"0\" y true 2 x 3", "^^ x \"0\" y true 2 z x 3"],
		mlton
	),
	FluentAPITestCase (
		"XYZ_Tabulation",
		xyz_fsm,
		Tabulation,
		["^^ $$", "^^ x 0 y 1 2 z $$", "^^ x \"0\" y true 2 z x \"3\" y false 5 z $$"],
		["^^ x 0 $$", "^^ x 0 x 1 y true 3 z $$", "^^ x \"0\" y true 2 x 3",
		 "^^ x \"0\" y true 2 z x 3 y false 5 z $$"],
		mlton
	),
	FluentAPITestCase (
		"HTML_Sparse",
		html_fsm,
		Sparse,
		["^^ html body done done $$",
		 "^^ html body h1 \"zzz...\" p \"wake up!\" done done $$",
		 "^^ " ^
		 "html " ^
		 "  body " ^
		 "    h1 \"Marine Life\" " ^
		 "    p \"Marine life is cool.\" " ^
		 "    table " ^
		 "      tr " ^
		 "        th \"Link\" " ^
		 "        th \"Teaser\" " ^
		 "      done " ^
		 "      tr " ^
		 "        td " ^
		 "          a \"https://en.wikipedia.org/wiki/Marine life\" \"Marine life on Wikipedia\" " ^
		 "        done " ^
		 "        td " ^
		 "          img \"https://tinyurl.com/cooldolfin\" " ^
		 "        done " ^
		 "      done " ^
		 "    done " ^
		 "  done " ^
		 "done " ^
		 "$$"],
		["^^ html html done done", "^^ p \"no body?\"", "^^ html html"],
		mlton
	)
]

(*************
 * Test Suit *
 *************)

val flunct_test_suit = Suit ("flunct tests", List.concat (map get_tests test_cases))

end
