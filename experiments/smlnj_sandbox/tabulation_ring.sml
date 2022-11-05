structure Ring = struct

datatype $ = $
fun I0 (x0, x1, x2, x3) = x0
fun I1 (x0, x1, x2, x3) = x1
fun I2 (x0, x1, x2, x3) = x2
fun I3 (x0, x1, x2, x3) = x3
val e0 = ($, I2)
val e1 = ((), I0)
val e2 = ((), I3)
val e3 = ((), I1)
val e = (e0, e1, e2, e3)
datatype call = A
val ^^ = fn f => f { id = e0, calls = [] : call list }
fun a { id = (_, I), calls : call list } = fn f => f { id = I e, calls = A::calls }
fun $$ { id = ($, _), calls : call list } = rev calls

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
val _ = print_time (t2 - t1);
val _ = print_time (t4 - t3);
val _ = print_time (t6 - t5);
val _ = print_time (t8 - t7);
val _ = OS.Process.exit OS.Process.success;
