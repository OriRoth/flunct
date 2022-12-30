structure A4 = struct

datatype t = T
datatype f = F
datatype call = A
datatype ('f8, 'f1, 'f2, 'f4, 'f15) id = ID
fun a { id : ('f8, 'f1, 'f2, 'f4, t) id, controls = (), calls : call list } = fn f => f { id = ID : ('f1, 'f2, 'f4, 'f8, t) id, controls = (), calls = A ::calls }
val ^^ = fn f => f { id = ID : (t, f, f, f, t) id, controls = (), calls = [] : call list }
fun $$ { id : (t, 'p2, 'p3, 'p4, 'p5) id, controls = (), calls : call list } = rev calls

end
