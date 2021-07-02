use "adv10.sml";                (* knotHash *)

val key = "jzgqcdpd"

(* First part *)

fun row n = let val str = key ^ "-" ^ Int.toString n
            in map ord (explode str) end

val rows = Vector.tabulate (128, knotHash o row)

fun ones #"0" = 0 | ones #"1" = 1 | ones #"2" = 1 | ones #"3" = 2
  | ones #"4" = 1 | ones #"5" = 2 | ones #"6" = 2 | ones #"7" = 3
  | ones #"8" = 1 | ones #"9" = 2 | ones #"a" = 2 | ones #"b" = 3
  | ones #"c" = 2 | ones #"d" = 3 | ones #"e" = 3 | ones #"f" = 4
  | ones _ = raise Fail "invalid digit"

fun countSquares hash = foldr op + 0 (map ones (explode hash))

fun adv14 () = Vector.foldr op + 0 (Vector.map countSquares rows)

(* Second part *)

fun bit c i =
    let val hex = if c > #"9" then 10 + ord c - ord #"a"
                  else ord c - ord #"0"
        val mask = Word.<< (Word.fromInt 1, Word.fromInt (3 - i))
    in Word.toInt (Word.andb (Word.fromInt hex, mask)) > 0 end

fun used (i, j) =
    let val r = Vector.sub (rows, i)
    in bit (String.sub (r, j div 4)) (j mod 4) end

fun initMap () =
    let val n = ref 0
        fun f pos = if used pos then ( n := !n + 1 ; !n ) else 0
    in Array2.tabulate Array2.RowMajor (128, 128, f) end

fun neighbors arr (i, j) =
    let fun f (di, dj) =
            let val n = Array2.sub (arr, i + di, j + dj) handle Subscript => 0
            in if n = 0 then NONE else SOME n end
    in List.mapPartial f [(~1, 0), (0, 1), (1, 0), (0, ~1)] end

fun mergeGroups arr =
    let val changed = ref false
        val range = { base = arr, row = 0, col = 0, nrows = NONE, ncols = NONE }
        fun f (_,_,0) = 0
          | f (i,j,n) =
            let val m = foldr Int.min n (neighbors arr (i, j))
            in if m < n then ( changed := true ; m ) else n end
    in Array2.modifyi Array2.RowMajor f range
     ; if !changed then mergeGroups arr else ()
    end

fun countGroups arr =
    let val seen = ref []
        fun f 0 = () |
            f n = if List.exists (fn k => k = n) (!seen) then ()
                  else seen := n :: !seen
    in Array2.app Array2.RowMajor f arr ; length (!seen) end

fun adv14b () =
    let val arr = initMap ()
    in mergeGroups arr ; countGroups arr end
