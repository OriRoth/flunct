structure BinaryTest = struct
open Test
open Binary

val binary_test_suit = Suit ("binary tests", [
	Test ("or", fn () => (let
		val _ = assert "[] | [] = []"
			([] = (or [] []))
		val _ = assert "[0, 0, 1, 1] | [0, 1, 0, 1] = [0, 1, 1, 1]"
			([B0, B1, B1, B1] = (or [B0, B0, B1, B1] [B0, B1, B0, B1]))
	in () end)),
	Test ("arrays", fn () => (let
		val _ = arrays_memoization := [] (* clear memoization *)
		val _ = assert "arrays of length 0 are [[]]"
			([[]] = (arrays 0))
		val _ = assert "arrays of length 1 are [[0], [1]]"
			([[B0], [B1]] = (arrays 1))
		val _ = assert "arrays of length 2 are [[0, 0], [0, 1], [1, 0], [1, 1]]"
			([[B0, B0], [B0, B1], [B1, B0], [B1, B1]] = (arrays 2))
		val _ = assert "1024 arrays of length 10"
			(1024 = (length (arrays 10)))
		val _ = assert "at this point 4 arrays should be memoized"
			(4 = (Map.size (!arrays_memoization)))
	in () end)),
	Test ("matrix_get", fn () => (let
		val _ = assert "[0, 1][1] = 1"
			(B1 = (matrix_get [B0, B1] [B1]))
		val _ = assert "[0, 0, 1, 0][0, 1] = 0"
			(B0 = (matrix_get [B0, B0, B1, B0] [B0, B1]))
		val _ = assert "[0, 0, 1, 0][1, 0] = 1"
			(B1 = (matrix_get [B0, B0, B1, B0] [B1, B0]))
		val _ = assert "[0, 0, 0, 0, 0, 1, 0, 0][0, 1, 0] = 0"
			(B0 = (matrix_get [B0, B0, B0, B0, B0, B1, B0, B0] [B0, B1, B0]))
		val _ = assert "[0, 0, 0, 0, 0, 1, 0, 0][1, 0, 1] = 1"
			(B1 = (matrix_get [B0, B0, B0, B0, B0, B1, B0, B0] [B1, B0, B1]))
	in () end)),
	Test ("matrix_set", fn () => (let
		val m = [B0,B0,B0,B0]
		val _ = assert "[0, 0, 0, 0][0, 0]:=1 = [1, 0, 0, 0]" ([B1, B0, B0, B0] = matrix_set m [B0, B0] B1)
		val _ = assert "[0, 0, 0, 0][0, 1]:=1 = [0, 1, 0, 0]" ([B0, B1, B0, B0] = matrix_set m [B0, B1] B1)
		val _ = assert "[0, 0, 0, 0][1, 0]:=1 = [0, 0, 1, 0]" ([B0, B0, B1, B0] = matrix_set m [B1, B0] B1)
		val _ = assert "[0, 0, 0, 0][1, 1]:=1 = [0, 0, 0, 1]" ([B0, B0, B0, B1] = matrix_set m [B1, B1] B1)
	in () end)),
	Test ("function_compose", fn () => (let
		val f0 = [B0, B0, B0, B0]   (* f(x, y) = 0 *)
		val f1 = [B1, B1, B1, B1]   (* f(x, y) = 1 *)
		val b0 = [B0, B0, B1, B1]   (* f(x, y) = x *)
		val b1 = [B0, B1, B0, B1]   (* f(x, y) = y *)
		val nb0 = [B1, B1, B0, B0]  (* f(x, y) = not x *)
		val nb1 = [B1, B0, B1, B0]  (* f(x, y) = not y *)
		val xor = [B0, B1, B1, B0]  (* f(x, y) = x xor y *)
		val xnor = [B1, B0, B0, B1] (* f(x, y) = x xnor y *)
		val or = [B0, B1, B1, B1]   (* f(x, y) = x or y *)
		val And = [B0, B0, B0, B1]  (* f(x, y) = x and y *)
		val nand = [B1, B1, B1, B0] (* f(x, y) = x nand y *)
		val _ = assert "f0 o gs = f0"
			(f0 = (function_compose f0 [f0, f1]) andalso
			f0 = (function_compose f0 [b0, b1]) andalso
			f0 = (function_compose f0 [xor, or]))
		val _ = assert "xor o [xor, nb0] = nb1"
			(nb1 = (function_compose xor [xor, nb0]))
		val _ = assert "or o [xor, nb0] = nand"
			(nand = (function_compose or [xor, nb0]))
		val _ = assert "nb0 o [xor, nb0] = xnor"
			(xnor = (function_compose nb0 [xor, nb0]))
		val _ = assert "And o [xor, xnor] = b0"
			(f0 = (function_compose And [xor, xnor]))
		val _ = assert "b0 o [f, g] = f"
			(f0 = (function_compose b0 [f0, f1]) andalso
			xor = (function_compose b0 [xor, xnor]) andalso
			And = (function_compose b0 [And, or]))
	in () end))
])

end
