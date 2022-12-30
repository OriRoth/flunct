structure Ring = struct

fun T x _ = x
fun F _ y = y
fun N p = p F T
fun P x y z = z x y
fun C p = p (P T T) (P F F)
datatype call = A
datatype ('b0, 'b1) id = ID of 'b0 * 'b1
fun a { id = ID (b0, b1), calls : call list } = fn f => f { id = ID ((C b0) (fn b0_0 => fn b0_0_ => (C b0_0_) (fn b0_1 => fn b0_1_ => (C b0_1_) (fn b0_2 => fn b0_3 => (C b1) (fn b1_0 => fn b1_0_ => (C b1_0_) (fn b1_1 => fn b1_1_ => (C b1_1_) (fn b1_2 => fn b1_3 => (((N b0_0) ((N b1_0)) F) T (b0_1 ((N b1_1)) F), (b0_2 ((N b1_2)) F) T (b0_3 (b1_3) F))))))))), calls = A::calls }
val ^^ = fn f => f { id = ID (F, F), calls = [] : call list }
fun $$ { id = ID (b0_0, b1_0), calls : call list } = let val _  = (((N b0_0) ((N b1_0)) F) 0 "") + 0 in rev calls end

end;

open Ring;
val time = Time.toMilliseconds o Time.now;
val print_time = print o (fn s => s ^ "\n") o Int.toString o (fn t => t div 5) o IntInf.toInt;
val t1 = time ();
val _ = fn () => ^^ $$;
val _ = fn () => ^^ $$;
val _ = fn () => ^^ $$;
val _ = fn () => ^^ $$;
val _ = fn () => ^^ $$;
val t2 = time ();
val t3 = time ();
val _ = fn () => ^^ a a a a $$;
val _ = fn () => ^^ a a a a $$;
val _ = fn () => ^^ a a a a $$;
val _ = fn () => ^^ a a a a $$;
val _ = fn () => ^^ a a a a $$;
val t4 = time ();
val _ = print "Fig. B.5. Chain length vs. compilation time with 4-state Ring FSM and SML/NJ";
val _ = print_time (t2 - t1);
val _ = print_time (t4 - t3);
val _ = OS.Process.exit OS.Process.success;
