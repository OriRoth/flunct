structure Ring = struct

datatype t = T
datatype f = F
datatype call = A
datatype ('f8, 'f4, 'f1, 'f2, 'f15) id = ID
fun a { id : ('f8, 'f4, 'f1, 'f2, t) id, controls = (), calls : call list } = fn f => f { id = ID : ('f4, 'f1, 'f2, 'f8, t) id, controls = (), calls = A ::calls }
val ^^ = fn f => f { id = ID : (t, f, f, f, t) id, controls = (), calls = [] : call list }
fun $$ { id : (t, 'p2, 'p3, 'p4, 'p5) id, controls = (), calls : call list } = rev calls

end;

open Ring;
val time = Time.toMilliseconds o Time.now;
fun print_time n t = let
		val n_s =  Int.toString n
		val t_s = (Int.toString o (fn t => t div 5) o IntInf.toInt) t
	in
		print ("(" ^ n_s ^ "," ^ t_s ^ ")\n")
	end;
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
val t5 = time ();
val _ = fn () => ^^ a a a a a a a a a a a a a a a a $$;
val _ = fn () => ^^ a a a a a a a a a a a a a a a a $$;
val _ = fn () => ^^ a a a a a a a a a a a a a a a a $$;
val _ = fn () => ^^ a a a a a a a a a a a a a a a a $$;
val _ = fn () => ^^ a a a a a a a a a a a a a a a a $$;
val t6 = time ();
val t7 = time ();
val _ = fn () => ^^ a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a $$;
val _ = fn () => ^^ a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a $$;
val _ = fn () => ^^ a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a $$;
val _ = fn () => ^^ a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a $$;
val _ = fn () => ^^ a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a $$;
val t8 = time ();
val t9 = time ();
val _ = fn () => ^^ a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a $$;
val _ = fn () => ^^ a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a $$;
val _ = fn () => ^^ a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a $$;
val _ = fn () => ^^ a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a $$;
val _ = fn () => ^^ a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a $$;
val t10 = time ();
val _ = print "Fig. B.5. Chain length vs. compilation time with 4-state Ring FSM and SML/NJ\n";
val _ = print_time 0 (t2 - t1);
val _ = print_time 4 (t4 - t3);
val _ = print_time 16 (t6 - t5);
val _ = print_time 64 (t8 - t7);
val _ = print_time 256 (t10 - t9);
val _ = OS.Process.exit OS.Process.success;
