fun parse f =
    case TextIO.inputLine f of
        NONE => []
      | SOME line => line :: parse f

fun generateList [] _ = []
  | generateList (x::xs) n =
    let fun f [] _ = []
          | f (y::ys) m = if y = #"#" then (m, n) :: f ys (m + 1)
                          else f ys (m + 1)
    in f (explode x) 0 @ generateList xs (n + 1) end

fun readData fname =
    let val f = TextIO.openIn fname
        val d = parse f
    in TextIO.closeIn f ; generateList d 0 end

(* First part *)

val start = (12, 12)

datatype direction = Up | Down | Left | Right

fun right Up    = Right
  | right Right = Down
  | right Down  = Left
  | right Left  = Up

fun left Up    = Left
  | left Left  = Down
  | left Down  = Right
  | left Right = Up

fun move (x, y) Up    = (x, y - 1)
  | move (x, y) Down  = (x, y + 1)
  | move (x, y) Left  = (x - 1, y)
  | move (x, y) Right = (x + 1, y)

fun remove xs (p : int * int) =
    let val (ps, ys) = List.partition (fn x => x = p) xs
    in if ps = [] then NONE
       else SOME ys
    end

val infections = ref 0

fun burst xs _ _ 0 = xs
  | burst xs pos dir n =
    case remove xs pos of
        NONE => let val dir' = left dir
                in infections := !infections + 1
                 ; burst (pos :: xs) (move pos dir') dir' (n - 1)
                end
      | SOME xs' => let val dir' = right dir
                    in burst xs' (move pos dir') dir' (n - 1) end

fun adv22 () = ( infections := 0
               ; burst (readData "adv22.txt") start Up 10000
               ; !infections
               )

(* Second part *)

fun comparePos ((a, b), (c, d)) = if a < c then LESS
                                  else if a > c then GREATER
                                  else if b < d then LESS
                                  else if b > d then GREATER
                                  else EQUAL

(* SML/NJ Util library *)
structure Map = RedBlackMapFn (struct
                                type ord_key = int * int
                                val compare = comparePos
                                end)

datatype state = Clean | Weakened | Infected | Flagged

fun generateMap xs = foldr (fn (x, m) => Map.insert (m, x, Infected)) Map.empty xs

fun reverse Up    = Down
  | reverse Down  = Up
  | reverse Left  = Right
  | reverse Right = Left

fun burst' m _ _ 0 = m
  | burst' m p d n =
    case Map.find (m, p) of
        NONE => let val d' = left d
                    val m' = Map.insert (m, p, Weakened)
                in burst' m' (move p d') d' (n - 1) end
      | SOME Weakened => let val m' = Map.insert (m, p, Infected)
                         in infections := !infections + 1
                          ; burst' m' (move p d) d (n - 1)
                         end
      | SOME Infected => let val d' = right d
                             val m' = Map.insert (m, p, Flagged)
                         in burst' m' (move p d') d' (n - 1) end
      | SOME Flagged => let val d' = reverse d
                            val m' = #1 (Map.remove (m, p))
                        in burst' m' (move p d') d' (n - 1) end
      | SOME Clean => raise Fail "invalid state"

fun adv22b () = ( infections := 0
                ; burst' (generateMap (readData "adv22.txt")) start Up 10000000
                ; !infections
                )
