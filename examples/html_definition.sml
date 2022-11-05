CM.make "../src/flunct.cm";
open Flunct
fun tagging name params = Letter' (name, params, ["S", "f'", "S1", "S2", "g1'", "g2'", "h'"], ["f' " ^ (Util.uppercase name) ^ "' S", "()", "S1", "S2", "g1'", "g2'", "h'"])
fun non_tagging name params = Letter' (name, params, ["c1", "c2", "c3", "c4", "c5", "c6", "c7"], ["c1", "c2", "c3", "c4", "c5", "c6", "c7"])
val @< = Letter' ("<", [], ["S", "_", "S1", "S2", "_", "_", "_"], ["S", "fn t => fn s => (t, s)", "S1", "S2", "fn s => ((), s)", "fn s => ((), s)", "fn s1 => fn s2 => s2"])
val @</ = Letter' ("</", [], ["S", "_", "S1", "S2", "_", "_", "_"], ["S", "fn (t:'t) => fn (_:'t,s) => s", "S1", "S2", "fn (_, s) => s", "fn (_, s) => s", "fn s1 => fn ((), s2') => (s1, s2')"])
val @> = non_tagging ">" []
val @/> = non_tagging "/>" []
val @` = non_tagging "`" ["string"]
val html = tagging "html" []
val body = tagging "body" []
val h1 = tagging "h1" []
val h2 = tagging "h2" []
val h3 = tagging "h3" []
val p = tagging "p" []
val i = tagging "i" []
val b = tagging "b" []
val a = tagging "a" []
val src = non_tagging "src" ["string"]
val img = non_tagging "img" []
val table = Letter' ("table", [], ["S", "f'", "S1", "S2", "g1'", "g2'", "_"], ["f' TABLE' S", "()", "g1' S1", "g2' S2", "()", "()", "()"])
val tr = Letter' ("tr", [], ["S", "f'", "(S1, S1')", "S2", "_", "_", "h'"], ["f' TR' S", "()", "(S1, S1')", "h' S1 S2", "()", "()", "()"])
val th = Letter' ("th", [], ["S", "f'", "(S1, S1')", "S2", "_", "_", "_"], ["f' TH' S", "()", "(((), S1), S1')", "S2", "()", "()", "()"])
val td = Letter' ("td", [], ["S", "f'", "S1", "((S2, S2'), S2'')", "_", "_", "_"], ["f' TD' S", "()", "S1", "(S2', S2'')", "()", "()", "()"])
val nonfixes = ((String.concatWith "\n") o (map (fn name => "nonfix " ^ name)))
	["<", ">"]
val tag_types = ((String.concatWith "\n") o (map (fn name => "datatype " ^ name ^ " = " ^ name ^ "'")) o (map Util.uppercase))
	["html", "body", "h1", "h2", "h3", "p", "i", "b", "a", "table", "tr", "th", "td"]
val fsm = Fsm' (
	State 100,
	[State 107],
	[
		(* html *)
		Transition' (State 100, @<, State 101),
		Transition' (State 101, html, State 102),
		Transition' (State 102, @>, State 103),
		Transition' (State 104, @</, State 105),
		Transition' (State 105, html, State 106),
		Transition' (State 106, @>, State 107),
		(* body *)
		Transition' (State 103, @<, State 201),
		Transition' (State 201, body, State 202),
		Transition' (State 202, @>, State 203),
		Transition' (State 203, @</, State 204),
		Transition' (State 204, body, State 205),
		Transition' (State 205, @>, State 104),
		(* text *)
		Transition' (State 203, @`, State 203),
		(* h1, h2, h3 *)
		Transition' (State 203, @<, State 301),
		Transition' (State 301, h1, State 302),
		Transition' (State 301, h2, State 302),
		Transition' (State 301, h3, State 302),
		Transition' (State 302, @>, State 303),
		Transition' (State 303, @`, State 304),
		Transition' (State 304, @</, State 305),
		Transition' (State 305, h1, State 306),
		Transition' (State 305, h2, State 306),
		Transition' (State 305, h3, State 306),
		Transition' (State 306, @>, State 203),
		(* p *)
		Transition' (State 301, p, State 402),
		Transition' (State 402, @>, State 403),
		Transition' (State 403, @</, State 404),
		Transition' (State 404, p, State 405),
		Transition' (State 405, @>, State 203),
		(* text *)
		Transition' (State 403, @`, State 403),
		(* i, b *)
		Transition' (State 403, @<, State 501),
		Transition' (State 501, i, State 502),
		Transition' (State 501, b, State 502),
		Transition' (State 502, @>, State 503),
		Transition' (State 503, @`, State 504),
		Transition' (State 504, @</, State 505),
		Transition' (State 505, i, State 506),
		Transition' (State 505, b, State 506),
		Transition' (State 506, @>, State 403),
		(* a *)
		Transition' (State 501, a, State 602),
		Transition' (State 602, src, State 603),
		Transition' (State 603, @>, State 604),
		Transition' (State 604, @`, State 605),
		Transition' (State 605, @</, State 606),
		Transition' (State 606, a, State 607),
		Transition' (State 607, @>, State 403),
		(* img *)
		Transition' (State 301, img, State 702),
		Transition' (State 702, src, State 703),
		Transition' (State 703, @/>, State 203),
		(* table *)
		Transition' (State 301, table, State 802),
		Transition' (State 802, @>, State 803),
		Transition' (State 804, @</, State 805),
		Transition' (State 805, table, State 806),
		Transition' (State 806, @>, State 203),
		(* tr0 *)
		Transition' (State 803, @<, State 901),
		Transition' (State 901, tr, State 902),
		Transition' (State 902, @>, State 903),
		Transition' (State 903, @</, State 904),
		Transition' (State 904, tr, State 905),
		Transition' (State 905, @>, State 804),
		(* th *)
		Transition' (State 903, @<, State 1001),
		Transition' (State 1001, th, State 1002),
		Transition' (State 1002, @>, State 1003),
		Transition' (State 1003, @`, State 1004),
		Transition' (State 1004, @</, State 1005),
		Transition' (State 1005, th, State 1006),
		Transition' (State 1006, @>, State 903),
		(* tr *)
		Transition' (State 804, @<, State 1101),
		Transition' (State 1101, tr, State 1102),
		Transition' (State 1102, @>, State 1103),
		Transition' (State 1103, @</, State 1104),
		Transition' (State 1104, tr, State 1105),
		Transition' (State 1105, @>, State 804),
		(* td *)
		Transition' (State 1103, @<, State 1201),
		Transition' (State 1201, td, State 1202),
		Transition' (State 1202, @>, State 203), (* recursion here *)
		Transition' (State 204, td, State 1203),
		Transition' (State 1203, @>, State 1103)
	]
);
val fsm_size' = let
		fun aux [] = []
		  | aux ((Transition' (q, _, p))::rest) = q::p::(aux rest)
	in
		length (Set.make ((fn (Fsm' (_, _, ts)) => aux ts) fsm))
	end;
print (nonfixes ^ "\n" ^ tag_types ^ "\n" ^
	(flunct_api' "HTML" fsm Sparse {
		additional_control_initial = ["()", "()", "()", "()", "()", "()", "()"]
	  , additional_control_final = ["()", "_", "_", "_", "_", "_", "_"]}
));
