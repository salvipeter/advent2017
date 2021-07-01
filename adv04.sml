fun parse f =
    case TextIO.inputLine f of
        NONE => []
      | SOME line => (String.tokens Char.isSpace line) :: parse f

fun readData fname =
    let val f = TextIO.openIn fname
        val d = parse f
    in TextIO.closeIn f ; d end

(* First part *)

fun unique [] = true
  | unique (x::xs) = List.find (fn y => x = y) xs = NONE andalso unique xs

fun adv04 () = (length o List.filter unique o readData) "adv04.txt"

(* Second part  *)

fun anagram x y =
    let val xs = ListMergeSort.sort op < (String.explode x) (* SML/NJ sort library *)
        val ys = ListMergeSort.sort op < (String.explode y)
    in xs = ys end

fun noAnagram [] = true
  | noAnagram (x::xs) = List.find (anagram x) xs = NONE andalso noAnagram xs

fun adv04b () = (length o List.filter noAnagram o readData) "adv04.txt"
