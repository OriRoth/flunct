(*
 * Run all project tests.
 *)
CM.make "flunct_test.cm";

val _ = Test.suits_run [
	MathTest.math_test_suit,
	UtilTest.util_test_suit,
	SetTest.set_test_suit,
	MapTest.map_test_suit,
	FsmTest.fsm_test_suit,
	BinaryTest.binary_test_suit,
	FlunctTest.flunct_test_suit
]
val _ = OS.Process.exit OS.Process.success
