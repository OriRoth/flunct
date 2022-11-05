structure MathTest = struct
open Test
open MathUtil

val math_test_suit = Suit ("math tests", [
	Test ("pow2", fn () => (let
		val _ = assert "2**0 = 1" (pow2 0 = 1)
		val _ = assert "2**1 = 2" (pow2 1 = 2)
		val _ = assert "2**2 = 4" (pow2 2 = 4)
		val _ = assert "2**10 = 1024" (pow2 10 = 1024)
		val _ = throws "2**-1 !!!" Domain (fn () => pow2 ~1)
	in () end)),
	Test ("is_pow2", fn () => (let
		val _ = assert "1 = 2**0" (is_pow2 1)
		val _ = assert "2 = 2**1" (is_pow2 2)
		val _ = assert "4 = 2**2" (is_pow2 4)
		val _ = assert "1024 = 2**10" (is_pow2 1024)
		val _ = assert "0 != 2**n" ((not o is_pow2) 0)
		val _ = assert "3 != 2**n" ((not o is_pow2) 3)
		val _ = assert "10 != 2**n" ((not o is_pow2) 10)
		val _ = assert "1023 != 2**n" ((not o is_pow2) 1023)
		val _ = throws "is_pow2 -1 !!!" Domain (fn () => is_pow2 ~1)
	in () end)),
	Test ("log2", fn () => (let
		val _ = assert "log2 1 = 0" (log2 1 = 0)
		val _ = assert "log2 8 = 3" (log2 8 = 3)
		val _ = assert "log2 7 = 2" (log2 7 = 2)
		val _ = throws "log2 0 !!!" Domain (fn () => log2 0)
		val _ = throws "log2 -1 !!!" Domain (fn () => log2 ~1)
	in () end))
])

end
