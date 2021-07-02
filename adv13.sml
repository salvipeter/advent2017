fun parseLine line =
    let val lineNoLF = substring (line, 0, size line - 1)
        val words = String.tokens Char.isSpace lineNoLF
        val p = valOf (Int.fromString (hd words))
        val q = valOf (Int.fromString (hd (tl words)))
    in (p, q) end

fun parse f =
    case TextIO.inputLine f of
        NONE => []
      | SOME line => (parseLine line) :: parse f

fun readData fname =
    let val f = TextIO.openIn fname
        val d = parse f
    in TextIO.closeIn f ; d end

(* First part *)

fun severity [] _ = 0
  | severity ((n,d)::xs) wait =
    if (n + wait) mod (2 * (d - 1)) = 0 then
        n * d + severity xs wait
    else
        severity xs wait

fun adv13 () = severity (readData "adv13.txt") 0

(* Second part *)

(* Like severity = 0, but also true if caught by sensor #0 *)
fun free [] _ = true
  | free ((n,d)::xs) wait =
    (n + wait) mod (2 * (d - 1)) > 0 andalso free xs wait

fun waitForIt scanners n =
    if free scanners n then n
    else waitForIt scanners (n + 1)

fun adv13b () = waitForIt (readData "adv13.txt") 0
