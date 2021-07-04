type point = int * int * int
type particle = point * point * point

fun isSeparator c = not (Char.isDigit c) andalso c <> #"-"

fun takeVec list n = case List.drop (list, n) of
                         (x::y::z::_) => (x, y, z)
                       | _ => raise Fail "invalid vector"

fun parseLine line =
    let val words = String.tokens isSeparator line
        val nums = map (valOf o Int.fromString) words
    in (takeVec nums 0, takeVec nums 3, takeVec nums 6) end

fun parse f =
    case TextIO.inputLine f of
        NONE => []
      | SOME line => (parseLine line) :: parse f

fun readData fname =
    let val f = TextIO.openIn fname
        val d = parse f
    in TextIO.closeIn f ; d end

(* First part *)

fun manhattan (x, y, z) = abs x + abs y + abs z

fun minIndex xs =
    let fun f [] _ _ mini = mini
          | f (x::xs) i min mini = if x < min then f xs (i + 1) x i
                                   else f xs (i + 1) min mini
    in f (tl xs) 1 (hd xs) 0 end

fun closest (ps : particle list) = minIndex (map (manhattan o #3) ps)

fun adv20 () = closest (readData "adv20.txt")

(* Second part *)

(* Position after n iterations: p + n * v + n (n + 1) / 2 * a *)

fun add (x, y, z) (dx, dy, dz) = (x + d, y + d, z + d)

fun update (pos, vel, acc) =
    let val vel' = add vel acc
        val pos' = add pos vel'
    in (pos', vel', acc) end

fun removeCollisions [] = []
  | removeCollisions (x::xs) =
    let fun collides (p : particle) = #1 p = #1 x
        val (coll, rest) = List.partition collides xs
    in if coll = [] then x :: removeCollisions xs
       else removeCollisions rest
    end

fun simulate ps = (
    (print o Int.toString o length) ps
  ; print "\n"
  ; (simulate o removeCollisions o map update) ps
)

fun adv20b () = simulate (readData "adv20.txt")
