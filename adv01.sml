fun parse f =
    let val c = valOf (TextIO.input1 f)
    in if Char.isDigit c then
           (ord c - ord #"0") :: parse f
       else
           []
    end

fun readData fname =
    let val f = TextIO.openIn fname
        val d = parse f
    in TextIO.closeIn f ; d end

(* First part *)

fun sumSame xs =
    let fun f [y] = if hd xs = y then y else 0
          | f (y::z::ys) = (if y = z then y else 0) + f (z::ys)
    in f xs end

fun adv01 () = sumSame (readData "adv01.txt")

(* Second part *)

fun captcha v =
    let val n = Vector.length v
        fun f ~1 = 0
          | f i = let val x = Vector.sub (v, i)
                      val j = (i + n div 2) mod n
                      val v = if x = Vector.sub (v, j) then x else 0
                  in v + f (i - 1) end
    in f (n - 1) end

fun adv01b () = (captcha o vector o readData) "adv01.txt"
