use "a4_api.sml";
open A4;

(* fluent chain examples *)
^^ $$; (* compiles, ε∈(a^4)* *)
^^ a a a a $$; (* compiles, aaaa∈(a^4)* *)
(*^^ a a a $$;*) (* does not compile, aaa∉(a^4)* *)
(*^^ a a a a a a a a a a a $$;*) (* does not compile, a^11∉(a^4)* *)
val _ = OS.Process.exit OS.Process.success;
