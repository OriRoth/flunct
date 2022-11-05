structure MapTest = struct
open Test
open Map

val map_test_suit = Suit ("map tests", [
	Test ("put", fn () => (let
		val _ = assert "[] + (1, 2) = [(1, 2)]"
			(put [] 1 2 = [(1, 2)])
		val _ = assert "[(1, 2)] + (1, 3) = [(1, 3)]"
			(put [(1, 2)] 1 3 = [(1, 3)])
		val _ = assert "[(1, 2)] + (3, 4) = [(1, 2), (3, 4)]"
			(equals (put [(1, 2)] 3 4) [(1, 2), (3, 4)])
		val _ = assert "[(1, 2), (3, 4)] + (3, 5) = [(1, 2), (3, 5)]"
			(equals (put [(1, 2), (3, 4)] 3 5) [(1, 2), (3, 5)])
	in () end)),
	Test ("contains", fn () => (let
		val _ = assert "[] does not contain 1"
			(not (contains [] 1))
		val _ = assert "[(1, 2)] contains 1"
			(contains [(1, 2)] 1)
		val _ = assert "[(1, 2)] does not contain 3"
			(not (contains [(1, 2)] 3))
		val _ = assert "[(1, 2), (3, 4)] contains 3"
			(contains [(1, 2), (3, 4)] 3)
	in () end)),
	Test ("get", fn () => (let
		val _ = assert "[] at 1 = none"
			((get [] 1) = NONE)
		val _ = assert "[(1, 2)] at 3 = none"
			((get [(1, 2)] 3) = NONE)
		val _ = assert "[(1, 2), (3, 4)] at 5 = none"
			((get [(1, 2), (3, 4)] 5) = NONE)
		val _ = assert "[(1, 2)] at 1 = 2"
			((get [(1, 2)] 1) = SOME 2)
		val _ = assert "[(1, 2), (3, 4)] at 3 = 4"
			((get [(1, 2), (3, 4)] 3) = SOME 4)
	in () end)),
	Test ("keys", fn () => (let
		val _ = assert "keys of [] = []"
			(keys [] = [])
		val _ = assert "keys of [(1, 2)] = [1]"
			(keys [(1, 2)] = [1])
		val _ = assert "keys of [(1, 2), (3, 4), (5, 6)] = [1, 3, 5]"
			(keys [(1, 2), (3, 4), (5, 6)] = [1, 3, 5])
	in () end)),
	Test ("values", fn () => (let
		val _ = assert "values of [] = []"
			(values [] = [])
		val _ = assert "values of [(1, 2)] = [2]"
			(values [(1, 2)] = [2])
		val _ = assert "values of [(1, 2), (3, 4), (5, 6)] = [2, 4, 6]"
			(values [(1, 2), (3, 4), (5, 6)] = [2, 4, 6])
	in () end))
])

end
