val startA = IntInf.fromInt 512
val startB = IntInf.fromInt 191

(* First part *)

val factorA = IntInf.fromInt 16807
val factorB = IntInf.fromInt 48271
val divisor = IntInf.fromInt 2147483647

fun generate n factor =
    n * factor mod divisor

fun judge a b = IntInf.andb (a, 0xffff) = IntInf.andb (b, 0xffff)

fun testSample _ _ 0 = 0
  | testSample a b n =
    let val a1 = generate a factorA
        val b1 = generate b factorB
    in if judge a1 b1 then 1 + testSample a1 b1 (n - 1)
       else testSample a1 b1 (n - 1)
    end

fun adv15 () = testSample startA startB 40000000

(* Second part *)

fun generate' n factor mask =
    let val try = n * factor mod divisor
    in if IntInf.andb (try, mask) = 0 then try
       else generate' try factor mask
    end

fun testSample' _ _ 0 = 0
  | testSample' a b n =
    let val a1 = generate' a factorA (IntInf.fromInt 3)
        val b1 = generate' b factorB (IntInf.fromInt 7)
    in if judge a1 b1 then 1 + testSample' a1 b1 (n - 1)
       else testSample' a1 b1 (n - 1)
    end

fun adv15b () = testSample' startA startB 5000000
