fun isSeparator x = x = #" " orelse x = #","

fun parseLine line =
    let val lineNoLF = substring (line, 0, size line - 1)
        val words = String.tokens isSeparator lineNoLF
        val p = valOf (Int.fromString (hd words))
        val ps = map (valOf o Int.fromString) (tl (tl words))
    in (p, ps) end

fun parse f =
    case TextIO.inputLine f of
        NONE => []
      | SOME line => (parseLine line) :: parse f

fun readData fname =
    let val f = TextIO.openIn fname
        val d = parse f
    in TextIO.closeIn f ; d end

(* First part *)

fun collectGroup _ (group : int list) [] = group
  | collectGroup data group (x::xs) =
    if List.exists (fn y => y = x) group then
        collectGroup data group xs
    else
        let val (_, ys) = valOf (List.find (fn (y, _) => y = x) data)
        in collectGroup data (x :: group) (xs @ ys) end

fun adv12 () = length (collectGroup (readData "adv12.txt") [] [0])

(* Second part *)

fun subtractGroup (_ : int list) [] = []
  | subtractGroup group ((x,ys)::xs) =
    if List.exists (fn y => y = x) group then
        subtractGroup group xs
    else
        (x, ys) :: subtractGroup group xs

fun allGroups [] = []
  | allGroups data =
    let val (first, _) = hd data
        val group = collectGroup data [] [first]
    in group :: allGroups (subtractGroup group data) end

fun adv12b () = (length o allGroups o readData) "adv12.txt"
