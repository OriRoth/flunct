structure CLI = struct

datatype t = T
datatype f = F
datatype call = Name of string | Description of string | Optional | Argument of string | @|| of string | Argtype of string
datatype ('f120, 'f128, 'f88, 'f0, 'f124, 'f24, 'f16, 'f252) id = ID
fun name { id : ('f120, t, 'f88, 'f0, 'f124, 'f24, 'f16, 'f252) id, controls = (), calls : call list } = fn x0 => fn f => f { id = ID : (t, 'f0, t, 'f0, t, 'f0, 'f0, t) id, controls = (), calls = (Name (x0))::calls }
fun description { id : ('f120, 'f128, t, 'f0, 'f124, 'f24, 'f16, 'f252) id, controls = (), calls : call list } = fn x0 => fn f => f { id = ID : (t, 'f0, 'f0, 'f0, t, 'f0, 'f0, t) id, controls = (), calls = (Description (x0))::calls }
fun optional { id : (t, 'f128, 'f88, 'f0, 'f124, 'f24, 'f16, 'f252) id, controls = (), calls : call list } = fn f => f { id = ID : ('f0, 'f0, 'f0, 'f0, t, 'f0, 'f0, t) id, controls = (), calls = Optional ::calls }
fun argument { id : ('f120, 'f128, 'f88, 'f0, t, 'f24, 'f16, 'f252) id, controls = (), calls : call list } = fn x0 => fn f => f { id = ID : (t, 'f0, t, 'f0, t, t, t, t) id, controls = (), calls = (Argument (x0))::calls }
fun || { id : ('f120, 'f128, 'f88, 'f0, 'f124, t, 'f16, 'f252) id, controls = (), calls : call list } = fn x0 => fn f => f { id = ID : (t, 'f0, t, 'f0, t, t, 'f16, t) id, controls = (), calls = (@|| (x0))::calls }
fun argtype { id : ('f120, 'f128, 'f88, 'f0, 'f124, 'f24, t, 'f252) id, controls = (), calls : call list } = fn x0 => fn f => f { id = ID : (t, 'f0, t, 'f0, t, t, 'f0, t) id, controls = (), calls = (Argtype (x0))::calls }
val ^^ = fn f => f { id = ID : (f, t, f, f, f, f, f, t) id, controls = (), calls = [] : call list }
fun $$ { id : (t, 'p2, 'p3, 'p4, 'p5, 'p6, 'p7, 'p8) id, controls = (), calls : call list } = rev calls

end

val icecream_cli = let
		open CLI
		val cli = ^^
		val build = $$
	in
		cli
			name "Ice Cream Machine"
			description "Makes customized ice cream."
			argument "--flavors" || "-f"
				argtype "@list"
				description "list of icecream flavors"
			optional argument "--toppings" || "-t"
				argtype "@list"
				description "list of icecream flavors"
			optional argument "--container" || "-c"
				argtype "cone" || "cup" || "styrofoam"
		build
	end
