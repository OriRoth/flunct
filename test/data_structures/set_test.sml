structure SetTest = struct
open Test
open Set

val set_test_suit = Suit ("set tests", [
	Test ("contains", fn () => (let
		val _ = assert "1 in [1, 2, 3]"
			(contains [1, 2, 3] 1)
		val _ = assert "3 in [1, 2, 3]"
			(contains [1, 2, 3] 3)
		val _ = assert "4 in [2, 3, 4]"
			(contains [2, 3, 4] 4)
		val _ = assert "0 not in []"
			(not (contains [] 0))
		val _ = assert "1 not in []"
			(not (contains [] 1))
		val _ = assert "4 not in [1, 2, 3]"
			(not (contains [1, 2, 3] 4))
		val _ = assert "1 not in [2, 3, 4]"
			(not (contains [2, 3, 4] 1))
		val _ = assert "5 not in [2, 3, 4]"
			(not (contains [2, 3, 4] 5))
	in () end)),
	Test ("size", fn () => (let
		val _ = assert "|[]| = 0"
			((size []) = 0)
		val _ = assert "|[1, 2, 3]| = 3"
			((size [1, 2, 3]) = 3)
		val _ = assert "|[1, 2, 3, 4]| = 4"
			((size [1, 2, 3, 4]) = 4)
	in () end)),
	Test ("insert", fn () => (let
		val _ = assert "[] + 1 = [1]"
			((insert [] 1) = [1])
		val _ = assert "[1] + 1 = [1]"
			((insert [1] 1) = [1])
		val _ = assert "[1] + 2 = [1, 2]"
			(equals (insert [1] 2) [1, 2])
	in () end)),
	Test ("insert_all", fn () => (let
		val _ = assert "[] + [] = []"
			((insert_all [] []) = [])
		val _ = assert "[] + [1] = [1]"
			((insert_all [] [1]) = [1])
		val _ = assert "[] + [1, 2] = [1, 2]"
			(equals (insert_all [] [1, 2]) [1, 2])
		val _ = assert "[1, 2] + [3, 4] = [1, 2, 3, 4]"
			(equals (insert_all [1, 2] [3, 4]) [1, 2, 3, 4])
		val _ = assert "[1, 2] + [2, 3] = [1, 2, 3]"
			(equals (insert_all [1, 2] [2, 3]) [1, 2, 3])
	in () end)),
	Test ("contains_all", fn () => (let
		val _ = assert "[] in []"
			(contains_all [] [])
		val _ = assert "[] in [1]"
			(contains_all [1] [])
		val _ = assert "[1] in [1]"
			(contains_all [1] [1])
		val _ = assert "[1] in [1, 2]"
			(contains_all [1, 2] [1])
		val _ = assert "[1, 2] in [1, 2]"
			(contains_all [1, 2] [1, 2])
		val _ = assert "[1, 2] in [1, 2, 3, 4]"
			(contains_all [1, 2, 3, 4] [1, 2])
		val _ = assert "[1] not in []"
			(not (contains_all [] [1]))
		val _ = assert "[1] not in [2]"
			(not (contains_all [2] [1]))
		val _ = assert "[1, 2] not in [2, 3]"
			(not (contains_all [2, 3] [1, 2]))
	in () end)),
	Test ("equals", fn () => (let
		val _ = assert "[] = []"
			(equals [] [])
		val _ = assert "[1] = [1]"
			(equals [1] [1])
		val _ = assert "[1, 2] = [1, 2]"
			(equals [1, 2] [1, 2])
		val _ = assert "[1, 2] = [2, 1]"
			(equals [1, 2] [2, 1])
		val _ = assert "[1, 2, 3] = [2, 3, 1]"
			(equals [1, 2, 3] [2, 3, 1])
		val _ = assert "[] != [1]"
			(not (equals [] [1]))
		val _ = assert "[1] != []"
			(not (equals [1] []))
		val _ = assert "[1] != [2]"
			(not (equals [1] [2]))
		val _ = assert "[1] != [1, 2]"
			(not (equals [1] [1, 2]))
		val _ = assert "[1, 2] != [1]"
			(not (equals [1, 2] [1]))
	in () end)),
	Test ("make", fn () => (let
		val _ = assert "[] as set is []"
			(equals [] (make []))
		val _ = assert "[1] as set is [1]"
			(equals [1] (make [1]))
		val _ = assert "[1, 1] as set is [1]"
			(equals [1] (make [1, 1]))
		val _ = assert "[1, 2, 1] as set is [1, 2]"
			(equals [1, 2] (make [1, 2, 1]))
		val _ = assert "[3, 2, 2, 3, 1, 2, 2, 1] as set is [1, 2, 3]"
			(equals [1, 2, 3] (make [3, 2, 2, 3, 1, 2, 2, 1]))
	in () end)),
	Test ("subtract", fn () => (let
		val _ = assert "[] / [] = []"
			(equals [] (subtract [] []))
		val _ = assert "[] / [1, 2, 3] = []"
			(equals [] (subtract [] [1, 2, 3]))
		val _ = assert "[1] / [2] = [1]"
			(equals [1] (subtract [1] [2]))
		val _ = assert "[1] / [1] = []"
			(equals [] (subtract [1] [1]))
		val _ = assert "[1, 2, 3, 4] / [3, 4, 5] = [1, 2]"
			(equals [1, 2] (subtract [1, 2, 3, 4] [3, 4, 5]))
	in () end)),
	Test ("disjoint", fn () => (let
		val _ = assert "[] [] are disjoint" (disjoint [] [])
		val _ = assert "[1] [] are disjoint" (disjoint [1] [])
		val _ = assert "[1] [1] are not disjoint" (not (disjoint [1] [1]))
		val _ = assert "[1, 2, 3] [2, 3, 4] are not disjoint" (not (disjoint [1, 2, 3] [2, 3, 4]))
	in () end))
])

end
