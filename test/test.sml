(*
 * Testing utilities.
 *)
structure Test = struct
open Util

(*
 * Failed assertion error.
 *)
exception AssertionError of
	string (* failure message *)

(*
 * Makes an assertion.
 * Throws AssertionError if the assertion is false.
 *)
fun assert message false = raise AssertionError message
  | assert _ true = ()

exception DummyException_

(*
 * Asserts that callback c throws exception e.
 * Throws AssertionError if no exception is thrown.
 *)
fun throws message e c =
	(c (); raise DummyException_)
	handle e' => if exnName e = exnName e' then () else raise AssertionError message

(*
 * Possible test results.
 *)
datatype result =
	Success of (* test passed *)
		string (* test name *)
  | Failure of (* test failed *)
  		string (* test name *)
	  * string (* failure reason *)

(*
 * Check whether the result is Success.
 *)
fun result_success (Success _) = true
  | result_success (Failure (_, _)) = false

(*
 * A single test method.
 *)
datatype test =
	Test of
		string          (* test name *)
	  * (unit -> unit) (* test method *)

(*
 * Runs a single test.
 *)
fun test_run (Test (name, method)) =
	let
		val _ = method ()
	in
		Success name
	end handle
		AssertionError message => Failure (name, message)
	  | error => Failure (name, (exnMessage error))

(*
 * A test suit.
 *)
datatype suit =
	Suit of
		string     (* suit name *)
	  * test list (* suit's tests *)

(*
 * Runs a whole test suit and prints the results.
 * Returns true if all tests passed, otherwise false.
 *)
fun suit_run (Suit (name, tests)) =
	let
		fun print_test (Success name) = General.before (true, print ((text_green ("Test " ^ name ^ " passed!")) ^ "\n"))
		  | print_test (Failure (name, reason)) = General.before (false, print ((text_red ("Test " ^ name ^ " failed: ")) ^ reason ^ "\n"))
		val results = map (print_test o test_run) tests
		val all_passed = List.all id results
	in
		print ((text_background_magenta ((if all_passed then text_green else text_red)
				("Suit " ^ name ^ (if all_passed then " passed!" else " failed")))) ^ "\n");
		all_passed
	end

(*
 * Run multiple test suits.
 *)
fun suits_run suits =
	print ((text_background_yellow
		(if List.all id (map suit_run suits) then "All tests passed!" else text_red "Some tests failed")) ^ "\n")

end
