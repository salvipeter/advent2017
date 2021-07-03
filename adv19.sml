fun parse f =
    case TextIO.inputLine f of
        NONE => []
      | SOME line => line :: parse f

fun readData fname =
    let val f = TextIO.openIn fname
        val d = parse f
    in TextIO.closeIn f ; Array.fromList d end

(* First part *)

val start = (0, 75)
val goal = (199, 0)

fun get track (x, y) = String.sub (Array.sub (track, x), y)

fun nextPos (x, y) (dx, dy) = (x + dx, y + dy)

fun turnRight (0, 1) = (1, 0)
  | turnRight (1, 0) = (0, ~1)
  | turnRight (0, ~1) = (~1, 0)
  | turnRight (~1, 0) = (0, 1)
  | turnRight _ = raise Fail "invalid direction"

fun turnLeft (0, 1) = (~1, 0)
  | turnLeft (~1, 0) = (0, ~1)
  | turnLeft (0, ~1) = (1, 0)
  | turnLeft (1, 0) = (0, 1)
  | turnLeft _ = raise Fail "invalid direction"

(* Check the two possible direction, and choose the one that is not a #" " *)
fun turn track pos dir =
    let val left = nextPos pos (turnLeft dir)
        val right = nextPos pos (turnRight dir)
    in if get track left = #" " then
           (right, turnRight dir)
       else
           (left, turnLeft dir)
    end

fun followTrack track pos dir =
    if pos = goal then []
    else let val c = get track pos
         in if Char.isUpper c then
                c :: followTrack track (nextPos pos dir) dir
            else if c = #"+" then
                let val (pos', dir') = turn track pos dir
                in followTrack track pos' dir' end
            else
                followTrack track (nextPos pos dir) dir
         end

fun adv19 () = implode (followTrack (readData "adv19.txt") start (1, 0))

(* Second part *)

fun countTrack track pos dir =
    if pos = goal then 0
    else if get track pos = #"+" then
        let val (pos', dir') = turn track pos dir
        in 1 + countTrack track pos' dir' end
    else
        1 + countTrack track (nextPos pos dir) dir

fun adv19b () = countTrack (readData "adv19.txt") start (1, 0)
