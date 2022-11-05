structure DcflExperiment = struct
open Util
open Flunct
open ExperimentUtil

val elm_dyck_api =
	  "type DD = DD\n"
	^ "i1 (x1, x2, x3) = x1\n"
	^ "i2 (x1, x2, x3) = x2\n"
	^ "i3 (x1, x2, x3) = x3\n"
	^ "z = (DD, (i1, ()), ())\n"
	^ "s x = ((), (i2, i3), (x))\n"
	^ "r1 () = s(z)\n"
	^ "r2 (x) = s(s(x))\n"
	^ "r3 (x) = x\n"
	^ "rs = (r1, r2, r3)\n"
	^ "cc f = f z\n"
	^ "l (_, (i, _), x) f = f (i rs x)\n"
	^ "r (_, (_, i), x) f = f (i rs x)\n"
	^ "d (DD, _, _) = ()\n"

val sml_dyck_api =
	  "datatype $$ = $$\n"
	^ "fun I1 (x1, x2, x3) = x1\n"
	^ "fun I2 (x1, x2, x3) = x2\n"
	^ "fun I3 (x1, x2, x3) = x3\n"
	^ "val z = ($$, (I1, ()), ())\n"
	^ "fun s x = ((), (I2, I3), (x))\n"
	^ "fun r1 () = s(z)\n"
	^ "fun r2 (x) = s(s(x))\n"
	^ "fun r3 (x) = x\n"
	^ "val r = (r1, r2, r3)\n"
	^ "fun ^^ f' = f' z\n"
	^ "fun L (_, (I, _), X) f' = f' (I r X)\n"
	^ "fun R (_, (_, I), X) f' = f' (I r X)\n"
	^ "fun $ ($$, _, _) = ()\n"

datatype compiler = MLton | ELM

fun run_time MLton input_length = let
			val _ = if input_length mod 2 <> 0 then raise Fail "input length must be even" else ()
			val n = input_length div 2 
			val program = sml_dyck_api ^ "val _ = ^^ " ^ (repeat n "L ") ^ (repeat n "R ") ^ "$\n"
		in
			time_mlton program
		end
  | run_time compiler input_length = let
			val _ = if input_length mod 2 <> 0 then raise Fail "input length must be even" else ()
			val n = input_length div 2 
			val program = elm_dyck_api ^ "w = cc " ^ (repeat n "l ") ^ (repeat n "r ") ^ "d\n"
		in
			time_elm program
		end

end
