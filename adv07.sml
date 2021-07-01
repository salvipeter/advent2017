type disc = { name : string, weight : int, below : string list }

fun isSeparator c =
    List.exists (fn x => c = x) [#" ", #"(", #")", #","]

fun parseLine line =
    let val lineNoLF = substring (line, 0, size line - 1)
        val words = String.tokens isSeparator lineNoLF
    in { name = hd words
       , weight = valOf (Int.fromString (hd (tl words)))
       , below = if length words = 2 then []
                 else List.drop (words, 3)
       }
    end

fun parse f =
    case TextIO.inputLine f of
        NONE => []
      | SOME line => (parseLine line) :: parse f

fun readData fname =
    let val f = TextIO.openIn fname
        val d = parse f
    in TextIO.closeIn f ; d end

(* First part *)

fun bottom (discs : disc list) =
    let fun above (x : disc) (y : disc) =
            List.exists (fn z => z = #name x) (#below y)
        fun isBottom (x : disc) = not (List.exists (above x) discs)
    in List.find isBottom discs end

fun adv07 () = (#name o valOf o bottom o readData) "adv07.txt"

(* Second part *)

exception BadWeight of int

fun mostSeen (a::b::c::_) = if a = b orelse a = c then a else b
  | mostSeen (a::_) = a         (* arbitrary when there are exactly 2 elements *)
  | mostSeen [] = 0

fun node (discs : disc list) name =
    valOf (List.find (fn x => #name x = name) discs)

fun weight discs rootName =
    let val root = node discs rootName
        val ws = map (weight discs) (#below root)
        val w = mostSeen ws
        val bad = List.find (fn (_, x) => x <> w)
                            (ListPair.zip (#below root, ws))
    in case bad of
           NONE => #weight root + w * length ws
         | SOME (name, bw) => let val badDisc = node discs name
                                  val correct = #weight badDisc + w - bw
                              in raise BadWeight correct end
    end

fun adv07b () = weight (readData "adv07.txt") "aapssr"
                handle BadWeight w => w
