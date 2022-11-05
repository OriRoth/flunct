structure UtilTest = struct
open Test
open Util

val util_test_suit = Suit ("util tests", [
	Test ("contains", fn () => (let
		val _ = assert "0 not in []" (not (contains [] 0))
		val _ = assert "0 in [0]" (contains [0] 0)
		val _ = assert "4 in [0, 1, 2, 3, 4]" (contains [0, 1, 2, 3, 4] 4)
		val _ = assert "5 not in [0, 1, 2, 3, 4]" (not (contains [0, 1, 2, 3, 4] 5))
	in () end)),
	Test ("reduce", fn () => (let
		val _ = assert "reduce 0 plus [1, 2, 3, 4] = 10" (reduce 0 MathUtil.plus [1, 2, 3, 4] = 10)
		val _ = assert "reduce [] (@) [[1], [2], [3], [4]] = [1, 2, 3, 4]"
			(reduce [] (fn x => fn y => x @ y) [[1], [2], [3], [4]] = [1, 2, 3, 4])
		fun xor x y = x andalso not y orelse not x andalso y
		val _ = assert "reduce f xor [t, f, f, t, t] = t"
			(reduce false xor [true, false, false, true, true])
		val _ = assert "reduce f xor [] = f" (not (reduce false xor []))
		val _ = assert "reduce t xor [] = t" (reduce true xor [])
	in () end)),
	Test ("remove", fn () => (let
		fun eq0 x = x = 0
		val _ = assert "[] - 0 = []" (remove eq0 [] = [])
		val _ = assert "[0] - 0 = []" (remove eq0 [0] = [])
		val _ = assert "[0, 1, 2, 0, 3, 0, 0, 0] - 0 = [1, 2, 3]" (remove eq0 [0, 1, 2, 0, 3, 0, 0, 0] = [1, 2, 3])
	in () end)),
	Test ("max", fn () => (let
		val _ = assert "max [1, 3, 2] = 3" (max [1, 3, 2] = 3)
		val _ = assert "max [1, 1, 1] = 1" (max [1, 1, 1] = 1)
		val _ = throws "max [] !!!" Empty (fn () => max [])
	in () end)),
	Test ("range", fn () => (let
		val _ = assert "0->4 = [0, 1, 2, 3]" (range 0 4 = [0, 1, 2, 3])
		val _ = assert "1->5 = [1, 2, 3, 4]" (range 1 5 = [1, 2, 3, 4])
		val _ = assert "1->1 = []" (range 1 1 = [])
		val _ = assert "1->0 = []" (range 1 0 = [])
	in () end)),
	Test ("repeat", fn () => (let
		val _ = assert "'0' * 5 = '00000'" (repeat 5 "0" = "00000")
		val _ = assert "'01' * 2 = '0101'" (repeat 2 "01" = "0101")
		val _ = assert "'0' * 1 = '0'" (repeat 1 "0" = "0")
		val _ = assert "'0' * 0 = ''" (repeat 0 "0" = "")
	in () end)),
	Test ("repeat_list", fn () => (let
		val _ = assert "[1, 2] * 3 = [1, 2, 1, 2, 1, 2]" (repeat_list 3 [1, 2] = [1, 2, 1, 2, 1, 2])
		val _ = assert "[] * 3 = []" (repeat_list 3 [] = [])
		val _ = assert "[1, 2] * 0 = []" (repeat_list 0 [1, 2] = [])
	in () end)),
	Test ("heads", fn () => (let
		val _ = assert "heads [[1, 2], [3, 4], [5, 6]] = [1, 3, 5]" (heads [[1, 2], [3, 4], [5, 6]] = [1, 3, 5])
		val _ = assert "heads [[1], [2], [3, 4, 5]] = [1, 2, 3]" (heads [[1], [2], [3]] = [1, 2, 3])
		val _ = assert "heads [] = []" (heads [] = [])
		val _ = throws "heads [[1], [], [3]] !!!" Domain (fn () => heads [[1], [], [3]])
	in () end)),
	Test ("tails", fn () => (let
		val _ = assert "tails [[1, 2, 3], [4, 5, 6]] = [[2, 3], [5, 6]]"
			(tails [[1, 2, 3], [4, 5, 6]] = [[2, 3], [5, 6]])
		val _ = assert "tails [[1, 2, 3], [4]] = [[2, 3], []]"
			(tails [[1, 2, 3], [4]] = [[2, 3], []])
		val _ = assert "tails [] = []" (tails [] = [])
		val _ = throws "tails [[1], [], [3]] !!!" Domain (fn () => tails [[1], [], [3]])
	in () end)),
	Test ("transpose", fn () => (let
		val _ = assert "[[1, 2], [3, 4]]T = [[1, 3], [2, 4]]"
			(transpose [[1, 2], [3, 4]] = [[1, 3], [2, 4]])
		val _ = assert "[[1, 2]]T = [[1], [2]]" (transpose [[1, 2]] = [[1], [2]])
		val _ = assert "[]T = []" (transpose [] = [])
		val _ = throws "[[1], [2, 3]]T !!!" Domain (fn () => transpose [[1], [2, 3]])
	in () end)),
	Test ("default", fn () => (let
		val _ = assert "1 || 2? = 2" (default 1 (SOME 2) = 2)
		val _ = assert "a || X? = a" (default "a" NONE = "a")
	in () end)),
	Test ("first_upper", fn () => (let
		val _ = assert "first_upper 'zzz' = 'Zzz'" (first_upper "zzz" = "Zzz")
	in () end))
])

end
