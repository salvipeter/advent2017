datatype move = Spin of int | Exchange of int * int | Partner of char * char

fun slashedPair str =
    if String.sub (str, 2) = #"/" then
        (substring (str, 1, 1), String.extract (str, 3, NONE))
    else
        (substring (str, 1, 2), String.extract (str, 4, NONE))

fun parseMove str =
    case String.sub (str, 0) of
        #"s" => (Spin o valOf o Int.fromString o String.extract) (str, 1, NONE)
      | #"x" => let val (a, b) = slashedPair str
                in Exchange (valOf (Int.fromString a), valOf (Int.fromString b)) end
      | #"p" => let val (a, b) = slashedPair str
                in Partner (String.sub (a, 0), String.sub (b, 0)) end
      | _ => raise Fail "invalid move type"

fun parse line =
    let val lineNoLF = substring (line, 0, size line - 1)
        val words = String.tokens (fn x => x = #",") lineNoLF
    in map parseMove words end

fun readData fname =
    let val f = TextIO.openIn fname
        val d = (parse o valOf o TextIO.inputLine) f
    in TextIO.closeIn f ; d end

(* First part *)

fun move state [] = state
  | move (arr, start) (Spin n :: xs) =
    move (arr, (start - n) mod 16) xs
  | move (arr, start) (Exchange (a, b) :: xs) =
    let val a' = (a + start) mod 16
        val b' = (b + start) mod 16
        val va = Array.sub (arr, a')
        val vb = Array.sub (arr, b')
    in Array.update (arr, a', vb)
     ; Array.update (arr, b', va)
     ; move (arr, start) xs
    end
  | move (arr, start) (Partner (va, vb) :: xs) =
    let fun find p = (#1 o valOf o Array.findi (fn (_, x) => x = p)) arr
        val a = find va
        val b = find vb
    in Array.update (arr, a, vb)
     ; Array.update (arr, b, va)
     ; move (arr, start) xs
    end

fun danceString (arr, start) =
    let val xs = List.tabulate (16, fn i => Array.sub(arr, i))
    in implode (List.drop (xs, start) @ List.take (xs, start)) end

fun adv16 () =
    let val arr = (Array.fromList o explode) "abcdefghijklmnop"
    in (danceString o move (arr, 0) o readData) "adv16.txt" end

(* Second part *)

fun findPeriod state n xs =
    let val state' = move state xs
    in if danceString state' = "abcdefghijklmnop" then n
       else findPeriod state' (n + 1) xs
    end

fun iterate state 0 _ = state
  | iterate state n xs = iterate (move state xs) (n - 1) xs

fun copyArray arr = Array.tabulate (Array.length arr, fn i => Array.sub (arr, i))

fun adv16b () =
    let val arr = (Array.fromList o explode) "abcdefghijklmnop"
        val moves = readData "adv16.txt"
        val period = findPeriod (copyArray arr, 0) 1 moves
    in (danceString o iterate (arr, 0) (1000000000 mod period)) moves end
