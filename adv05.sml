fun parse f =
    case TextIO.inputLine f of
        NONE => []
      | SOME line => (valOf o Int.fromString) line :: parse f

fun readData fname =
    let val f = TextIO.openIn fname
        val d = parse f
    in TextIO.closeIn f ; Array.fromList d end

(* First part *)

fun run arr ip = if ip < 0 orelse ip >= Array.length arr then 0
                 else let val jump = Array.sub (arr, ip)
                      in Array.update (arr, ip, jump + 1)
                       ; 1 + run arr (ip + jump)
                      end

fun adv05 () = run (readData "adv05.txt") 0

(* Second part *)

fun nextJump n = if n >= 3 then n - 1 else n + 1

fun run' arr ip = if ip < 0 orelse ip >= Array.length arr then 0
                  else let val jump = Array.sub (arr, ip)
                       in Array.update (arr, ip, nextJump jump) (* changed only this line *)
                        ; 1 + run' arr (ip + jump)
                       end

fun adv05b () = run' (readData "adv05.txt") 0

(* Computed in 0.5s using MLton, very slow in SML/NJ *)
val _ = (print o Int.toString o adv05b) ()
