fun readData fname =
    let val f = TextIO.openIn fname
        val line = valOf (TextIO.inputLine f)
        val d = String.tokens (fn x => x = #",") line
    in TextIO.closeIn f
     ; map (valOf o Int.fromString) d
    end

(* First part *)

fun reverse arr i l =
    if l < 2 then ()
    else let val j = (i + l - 1) mod 256
             val v1 = Array.sub (arr, i)
             val v2 = Array.sub (arr, j)
         in Array.update (arr, i, v2)
          ; Array.update (arr, j, v1)
          ; reverse arr ((i + 1) mod 256) (l - 2)
         end

(* Last parameter is for the iteration count of the second part *)
fun hash _ _ _ _ _ 0 = ()
  | hash arr ls' [] i skip n = hash arr ls' ls' i skip (n - 1)
  | hash arr ls' (l::ls) i skip n =
    ( reverse arr i l
    ; hash arr ls' ls ((i + l + skip) mod 256) (skip + 1) n
    )

fun adv10 () =
    let val lengths = readData "adv10.txt"
        val arr = Array.tabulate (256, fn x => x)
    in hash arr lengths lengths 0 0 1
     ; Array.sub (arr, 0) * Array.sub (arr, 1)
    end

(* Second part *)

fun readData' fname =
    let val f = TextIO.openIn fname
        val line = valOf (TextIO.inputLine f)
    in TextIO.closeIn f
     ; map ord (explode (substring (line, 0, size line - 1)))
    end

fun xor (a, b) =
    Word.toInt (Word.xorb (Word.fromInt a, Word.fromInt b))

fun denseHash [] = []
  | denseHash xs =
    foldr xor 0 (List.take (xs, 16)) :: denseHash (List.drop (xs, 16))

fun printHex xs =
    let val toLower = implode o map Char.toLower o explode
        fun twoChar s = if size s = 1 then "0" ^ s else s
    in concat (map (toLower o twoChar o Int.fmt StringCvt.HEX) xs) end

fun knotHash data =
    let val lengths = data @ [17, 31, 73, 47, 23]
        val arr = Array.tabulate (256, fn x => x)
    in hash arr lengths lengths 0 0 64
     ; let val list = List.tabulate (256, fn i => Array.sub (arr, i))
       in printHex (denseHash list) end
    end

fun adv10b () = knotHash (readData' "adv10.txt")
