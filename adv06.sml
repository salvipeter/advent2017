fun parse f =
    let val line = valOf (TextIO.inputLine f)
        val words = String.tokens Char.isSpace line
    in map (valOf o Int.fromString) words end

fun readData fname =
    let val f = TextIO.openIn fname
        val d = parse f
    in TextIO.closeIn f ; d end

(* First part *)

(* Splits xs at the first y element;
   clears its value and returns (left, right),
   where left is reversed *)
fun splitFirst xs y =
    let fun f (x::xs) acc =
            if x = y then (0 :: acc, xs)
            else f xs (x :: acc)
    in f xs [] end

(* Consumes ys and builds the result in the reversed xs,
   similarly to the functional queue data structure *)
fun distribute2 xs ys 0 = rev xs @ ys
  | distribute2 xs [] n = distribute2 [] (rev xs) n
  | distribute2 xs (y::ys) n = distribute2 ((y + 1) :: xs) ys (n - 1)

fun distribute xs =
    let val max = foldr Int.max (hd xs) xs
        val (left, right) = splitFirst xs max
    in distribute2 left right max end

fun run xs seen =
    if List.find (fn ys => ys = xs) seen <> NONE then 0
    else 1 + run (distribute xs) (xs :: seen)

fun adv06 () = run (readData "adv06.txt") []

(* Second part *)

fun findState xs goal =
    if xs = goal then 1
    else 1 + findState (distribute xs) goal

fun run' xs seen =
    if List.find (fn ys => ys = xs) seen <> NONE then
        findState (distribute xs) xs
    else
        run' (distribute xs) (xs :: seen)

fun adv06b () = run' (readData "adv06.txt") []
