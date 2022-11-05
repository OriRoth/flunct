structure FSM = struct

datatype t = T
datatype f = F
datatype call = Initial of int | Accepting of int list | Alphabet | @-- of string | @<-- of string list | Transitions | @>> of int | @--> of int
datatype ('f16384, 'f0, 'f2048, 'f256, 'f4096, 'f512, 'f1024, 'f8192, 'f3072, 'f32768, 'f65280, 'f3584) id = ID
fun initial { id : ('f16384, 'f0, 'f2048, 'f256, 'f4096, 'f512, 'f1024, 'f8192, 'f3072, t, 'f65280, 'f3584) id, controls = (), calls : call list } = fn x0 => fn f => f { id = ID : ('f0, 'f0, 'f0, 'f0, 'f0, 'f0, 'f0, t, 'f0, 'f0, t, 'f0) id, controls = (), calls = (Initial (x0))::calls }
fun accepting { id : ('f16384, 'f0, 'f2048, 'f256, 'f4096, 'f512, 'f1024, t, 'f3072, 'f32768, 'f65280, 'f3584) id, controls = (), calls : call list } = fn x0 => fn f => f { id = ID : ('f0, 'f0, 'f0, 'f0, t, 'f0, 'f0, 'f0, 'f0, 'f0, t, 'f0) id, controls = (), calls = (Accepting (x0))::calls }
fun alphabet { id : ('f16384, 'f0, 'f2048, 'f256, t, 'f512, 'f1024, 'f8192, 'f3072, 'f32768, 'f65280, 'f3584) id, controls = (), calls : call list } = fn f => f { id = ID : ('f0, 'f0, t, 'f0, 'f0, 'f0, 'f0, 'f0, t, 'f0, t, t) id, controls = (), calls = Alphabet ::calls }
fun -- { id : ('f16384, 'f0, 'f2048, 'f256, 'f4096, 'f512, 'f1024, 'f8192, 'f3072, 'f32768, 'f65280, t) id, controls = (), calls : call list } = fn x0 => fn f => f { id = ID : ('f0, 'f0, 'f0, 'f512, 'f0, 'f0, 'f3072, 'f0, 'f3072, 'f0, t, 'f3072) id, controls = (), calls = (@-- (x0))::calls }
fun <-- { id : ('f16384, 'f0, 'f2048, 'f256, 'f4096, 'f512, t, 'f8192, 'f3072, 'f32768, 'f65280, 'f3584) id, controls = (), calls : call list } = fn x0 => fn f => f { id = ID : ('f0, 'f0, t, 'f0, 'f0, 'f0, 'f0, 'f0, t, 'f0, t, t) id, controls = (), calls = (@<-- (x0))::calls }
fun transitions { id : ('f16384, 'f0, t, 'f256, 'f4096, 'f512, 'f1024, 'f8192, 'f3072, 'f32768, 'f65280, 'f3584) id, controls = (), calls : call list } = fn f => f { id = ID : (t, 'f0, 'f0, 'f0, 'f0, 'f0, 'f0, 'f0, 'f0, 'f0, t, 'f0) id, controls = (), calls = Transitions ::calls }
fun >> { id : (t, 'f0, 'f2048, 'f256, 'f4096, 'f512, 'f1024, 'f8192, 'f3072, 'f32768, 'f65280, 'f3584) id, controls = (), calls : call list } = fn x0 => fn f => f { id = ID : ('f0, 'f0, 'f0, 'f0, 'f0, t, 'f0, 'f0, 'f0, 'f0, t, t) id, controls = (), calls = (@>> (x0))::calls }
fun --> { id : ('f16384, 'f0, 'f2048, t, 'f4096, 'f512, 'f1024, 'f8192, 'f3072, 'f32768, 'f65280, 'f3584) id, controls = (), calls : call list } = fn x0 => fn f => f { id = ID : (t, 'f0, 'f0, 'f0, 'f0, 'f0, 'f0, 'f0, 'f0, 'f0, t, 'f0) id, controls = (), calls = (@--> (x0))::calls }
val ^^ = fn f => f { id = ID : (f, f, f, f, f, f, f, f, f, t, t, f) id, controls = (), calls = [] : call list }
fun $$ { id : (t, 'p2, 'p3, 'p4, 'p5, 'p6, 'p7, 'p8, 'p9, 'p10, 'p11, 'p12) id, controls = (), calls : call list } = rev calls

end

val fsm_fsm = let
		open FSM
		val fsm = ^^
		val build = $$
	in
		fsm
		initial 0
		accepting [3]
		alphabet
		-- "initial"<--["int"]
		-- "accepting"<--["int list"]
		-- "alphabet"
		-- "--"<--["string"]
		-- "<--"<--["string list"]
		-- "transitions"
		-- ">>"<--["int"]
		-- "-->"<--["int"]
		transitions
		>> 0--("initial")-->1
		>> 1--("accepting")-->2
		>> 2--("alphabet")-->3
		>> 3--("--")-->4
		>> 4--("--")-->4
		>> 4--("<--")-->3
		>> 3--("transitions")-->5
		>> 5--(">>")-->6
		>> 6--("--")-->7
		>> 7--("-->")-->5
		build
	end
