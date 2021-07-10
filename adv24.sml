fun parseLine line =
    let val words = String.tokens (fn c => c = #"/") line
        val nums = map (valOf o Int.fromString) words
    in (hd nums, hd (tl nums)) end

fun parse f =
    case TextIO.inputLine f of
        NONE => []
      | SOME line => (parseLine line) :: parse f

fun readData fname =
    let val f = TextIO.openIn fname
        val d = parse f
    in TextIO.closeIn f ; d end

(* First part *)

fun canConnect (to : int) (a, b) = if a = to then SOME b
                                   else if b = to then SOME a
                                   else NONE

fun allBridges ports =
    let fun f _ [] _ = [0]
          | f n (x::xs) ys =
            case canConnect n x of
                NONE => f n xs (x::ys)
              | SOME n' => let val notUsed = f n xs (x::ys)
                               val used = f n' (ys @ xs) []
                           in notUsed @ map (fn y => y + n + n') used end
    in f 0 ports [] end

fun adv24 () = let val bridges = allBridges (readData "adv24.txt")
               in foldr Int.max 0 bridges end

(* Second part *)

(* Same, but with (length, strength) pairs *)
fun allBridges' ports =
    let fun f _ [] _ = [(0,0)]
          | f n (x::xs) ys =
            case canConnect n x of
                NONE => f n xs (x::ys)
              | SOME n' => let val notUsed = f n xs (x::ys)
                               val used = f n' (ys @ xs) []
                           in notUsed @ map (fn (l, s) => (l + 1, s + n + n')) used end
    in f 0 ports [] end

fun adv24b () = let val bridges = allBridges' (readData "adv24.txt")
                    fun max ((a, b), (c, d)) =
                        if a > c orelse a = c andalso b > d then (a, b)
                        else (c, d)
                in #2 (foldr max (0, 0) bridges) end
