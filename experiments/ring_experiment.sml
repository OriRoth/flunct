structure RingExperiment = struct
open Util
open Flunct
open ExperimentUtil

fun ring n = Fsm (
		State 0,
		[State 0],
		(Transition (State (n - 1), Letter ("a", []), State 0)) ::
		(map (fn i =>
			Transition (State i, Letter ("a", []), State (i + 1))
		) (range 0 (n - 1)))
	)

fun run_time method fsm_size chain_length = let
		val fsm = ring fsm_size
		val name = "Ring"
		val fluent_api = flunct_api name fsm method
		val chain = "^^" ^ (String.concat (map (fn _ => " a") (range 0 chain_length))) ^ " $$"
		val program = fluent_api ^ "open " ^ name ^ ";\nval _ = " ^ chain ^ ";\n"
	in
		time_mlton program
	end

fun api_size method fsm_size = let
		val fsm = ring fsm_size
		val name = "Ring"
		val fluent_api = flunct_api name fsm method
	in
		size fluent_api
	end

end
