fun readData fname =
    let val f = TextIO.openIn fname
    in (explode o valOf o TextIO.inputLine) f
       before TextIO.closeIn f
    end

val garbage = ref 0

fun skipGarbage stream =
    case hd stream of
        #"!" => skipGarbage (tl (tl stream))
      | #">" => tl stream
      | _ => ( garbage := !garbage + 1   (* for the second part *)
             ; skipGarbage (tl stream)
             )

fun score stream depth =
    case hd stream of
        #"{" => score (tl stream) (depth + 1)
      | #"}" => if depth = 1 then 1
                else depth + score (tl stream) (depth - 1)
      | #"<" => score (skipGarbage (tl stream)) depth
      | _ => score (tl stream) depth

fun adv09 () = score (readData "adv09.txt") 0

fun adv09b () = ( garbage := 0 ; adv09 () ; !garbage )
