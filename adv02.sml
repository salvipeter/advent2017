fun parseInts line =
    let val words = String.tokens Char.isSpace line
    in map (valOf o Int.fromString) words end

fun parse f =
    case TextIO.inputLine f of
        NONE => []
      | SOME line => (parseInts line) :: parse f

fun readData fname =
    let val f = TextIO.openIn fname
        val d = parse f
    in TextIO.closeIn f ; d end

(* First part *)

fun range row =
    let val max = foldr Int.max (hd row) row
        val min = foldr Int.min (hd row) row
    in max - min end

fun checksum sheet = foldr op + 0 (map range sheet)

fun adv02 () = (checksum o readData) "adv02.txt"

(* Second part *)

fun evenlyDivisible x y =
    if y > x then evenlyDivisible y x
    else if x mod y = 0 then SOME (x div y)
    else NONE

fun quotient row =
    let fun f (_::xs) [] = f xs (tl xs)
          | f (x::xs) (y::ys) = case evenlyDivisible x y of
                                    NONE => f (x::xs) ys
                                  | SOME n => n
    in f row (tl row) end

fun checksum2 sheet = foldr op + 0 (map quotient sheet)

fun adv02b () = (checksum2 o readData) "adv02.txt"
