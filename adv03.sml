val n = 312051

(* First part *)

(* Largest k s.t. k(k+1) < n-1 *)
val k = floor ((Math.sqrt (4.0 * real n) - 1.0) / 2.0)

val d = n - 1 - k * (k + 1)
val s = if k mod 2 = 0 then ~1 else 1

val x = (k + 1) div 2 * s - Int.min (k + 1, d) * s
val y = (k + 1) div 2 * s - Int.max (0, d - k - 1) * s

val adv03 = abs x + abs y

(* Second part *)

fun nextStep (k, 0) = (0, k)
  | nextStep (0, k) = if k > 0 then (~k - 1, 0) else (1 - k, 0)

fun signDec n = n - Int.sign n

fun areaSum [] _ = 0
  | areaSum (((x,y),v)::ps) (x0,y0) =
    if abs (x - x0) <= 1 andalso abs (y - y0) <= 1 then
        v + areaSum ps (x0,y0)
    else
        areaSum ps (x0,y0)

fun spiral (ps as ((x,y),w) :: _) step last =
    if w > n then w
    else case step of
             (0, 0) => let val next = nextStep last
                       in spiral ps next next end
           | (k, 0) => let val q = (x + Int.sign k, y)
                           val v = areaSum ps q
                       in spiral ((q,v)::ps) (signDec k, 0) last end
           | (0, k) => let val q = (x, y + Int.sign k)
                           val v = areaSum ps q
                       in spiral ((q,v)::ps) (0, signDec k) last end

fun adv03b () = spiral [((0,0),1)] (1, 0) (1, 0)
