fun readData fname =
    let val f = TextIO.openIn fname
        val line = valOf (TextIO.inputLine f)
        val lineNoLF = substring (line, 0, size line - 1)
    in String.tokens (fn x => x = #",") lineNoLF
       before TextIO.closeIn f
    end

(* First part *)

val maxDistance = ref 0

fun distance (x, y) = Int.max (abs x, abs y)

fun follow [] pos = pos
  | follow (p::ps) (x, y) =
    let val (dx, dy) = case p of
                           "s"  => ( 1, ~1)
                         | "sw" => ( 0, ~1)
                         | "nw" => (~1,  0)
                         | "n"  => (~1,  1)
                         | "ne" => ( 0,  1)
                         | "se" => ( 1,  0)
                         | _ => raise Fail "invalid direction"
        val next = (x + dx, y + dy)
    in maxDistance := Int.max (!maxDistance, distance next) (* for the second part *)
     ; follow ps next end

fun adv11 () = let val pos = follow (readData "adv11.txt") (0, 0)
               in distance pos end

(* Second part *)

fun adv11b () = ( maxDistance := 0 ; adv11 () ; !maxDistance )
