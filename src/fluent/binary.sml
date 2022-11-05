(*
 * Binary encoding of FSMs.
 *)
structure Binary = struct
open Util
open MathUtil

(*
 * A logical bit.
 *)
datatype bit =
	B0  (* zero *)
  | B1  (* one *)

(*
 * Bitwise OR of two binary vectors.
 *)
fun or [] [] = []
  | or (B0::rest1) (B0::rest2) = B0::(or rest1 rest2)
  | or (_::rest1) (_::rest2) = B1::(or rest1 rest2)
  | or _ _ = raise Fail "argument dimensions do not match"

(*
 * Arrays memoization.
 *)
val arrays_memoization : (int, bit list list) Map.map ref = ref []

(*
 * Generates all possible binary arrays of the given length.
 * This function is memoized using arrays_memoization.
 *
 * arrays 2 = [[B0,B0],
 *             [B0,B1],
 *             [B1,B0],
 *             [B1,B1]]
 *)
fun arrays n =
		let
			fun aux 0 = [[]]
			  | aux i = let
					val Rec = aux (i - 1)
				in
					(map (fn a => B0::a) Rec) @ (map (fn a => B1::a) Rec)
				end
		in
			if n < 0 then raise Fail ("negative array length: " ^ (Int.toString n))
			else case Map.get (!arrays_memoization) n of
				SOME res => res
			  | NONE => let
					val res = aux n
					val _ = arrays_memoization := Map.put (!arrays_memoization) n res
				in
					res
				end
		end

(*
 * Generates all binary matrices of dimensions 2^n.
 * A matrix of dimensions 2^n is represented by an arrays of length 2^n.
 * 
 * matrices 2 = [[B0,B0,B0,B0],
 *               [B0,B0,B0,B1],
 *               [B0,B0,B1,B0],
 *               [B0,B0,B1,B1],
 *               [B0,B1,B0,B0],
 *               ...]
 *)
fun matrices n = arrays (pow2 n)

(*
 * Fetches matrix[I]. The matrix is binary and index I is an array of bits.
 *
 * matrix_get [B0,B1,B1,B0] [B0, B0] = B0
 * matrix_get [B0,B1,B1,B0] [B0, B1] = B1
 * matrix_get [B0,B1,B1,B0] [B1, B0] = B1
 * matrix_get [B0,B1,B1,B0] [B1, B1] = B0
 *)
fun matrix_get matrix I =
	let
		fun aux (a::b::rest) [B0] = a
		  | aux (a::b::rest) [B1] = b
		  | aux m (B0::rest) = aux m rest
		  | aux m (B1::rest) = aux (List.drop (m, (pow2 (length rest)))) rest
		  | aux _ _ = raise Fail "unreachable"
	in
		if pow2 (length I) <> length matrix then raise
			Fail ("indexing does not match matrix dimensions: "
				^ (Int.toString (pow2 (length I))) ^ " vs. " ^ (Int.toString (length matrix)))
		else aux matrix I
	end

(*
 * Sets the Ith cell of the matrix.
 *
 * matrix_set [B0,B1,B1,B0] [B1, B0] B0 = [B0,B1,B0,B0]
 *)
fun matrix_set matrix I value =
	let
		fun aux (_::b::rest) [B0] value = value::b::rest
		  | aux (a::_::rest) [B1] value = a::value::rest
		  | aux m (B0::rest) value = aux m rest value
		  | aux m (B1::rest) value = let val p = (pow2 (length rest)) in (List.take (m, p)) @ (aux (List.drop (m, p)) rest value) end
		  | aux _ _ _ = raise Fail "unreachable"
	in
		if pow2 (length I) <> length matrix then raise
			Fail ("indexing does not match matrix dimensions: "
				^ (Int.toString (pow2 (length I))) ^ " vs. " ^ (Int.toString (length matrix)))
		else aux matrix I value
	end

(*
 * A binary function is encoded by a matrix.
 * Calling a function f with arguments vector x returns f[x]
 * (see matrix_get).
 *)
val function = matrix_get

(*
 * Composes a binary function with a vector of functions.
 * Let the function dimensions be 2^n.
 * The vector contains n functions of dimensions 2^n.
 * The resulted matrix m = f o gs holds m[I] = f[gs[I]], where gs[I] is the vector
 * [g[I] for g in gs].
 * 
 * function_compose [B0, B0, B0, B1] [[B0, B1, B1, B0], [B0, B1, B1, B1]] = [B0,B1,B1,B0]
 * i.e., AND o [XOR, OR] = XOR
 *)
fun function_compose [] _ = raise Fail "empty function"
  | function_compose _ [] = raise Fail "empty functions"
  | function_compose f gs = let
		val n = log2 (length f)
		val _ = if length gs <> n then raise Fail "function dimensions do not match" else ()
		val _ = if not (List.all (fn l => l = (pow2 n)) (map length gs)) then
			raise Fail "function dimensions do not match" else ()
		val Is = arrays n
	in
		map (function f) (
			map (fn I =>
				map (fn g =>
					function g I
				) gs
			) Is)
	end

end
