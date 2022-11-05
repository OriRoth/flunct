(*
 * Math utilities.
 *)
structure MathUtil = struct

(*
 * Computs 2 to the power of non-negative n.
 * 
 * pow2 10 = 1024
 *)
fun pow2 n = let
		fun aux 0 = 1
		  | aux n = 2 * (aux (n - 1))
	in
		if n < 0 then raise Domain
		else aux n
	end

(*
 * Checks whether non-negative n is a power of 2.
 * 
 * is_pow2 32 = true
 *)
fun is_pow2 n = let
		fun aux 0 = false
		  | aux 1 = true
		  | aux n = if n mod 2 <> 0 then false else aux (n div 2)
	in
		if n < 0 then raise Domain
		else aux n
	end

(*
 * Computes base 2 log of positive n.
 *
 * log2 8 = 3
 *)
fun log2 n = let
		fun aux 1 = 0
		  | aux n = 1 + (aux (n div 2))
	in
		if n < 1 then raise Domain
		else aux n
	end

(*
 * op+ as a function.
 *)
fun plus x y = x + y

end
