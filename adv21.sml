fun parseLine line =
    let val words = String.tokens Char.isSpace line
        fun deleteSlash c = if c = #"/" then "" else Char.toString c
        fun chars s = explode (String.translate deleteSlash s)
    in (chars (hd words), chars (hd (tl (tl words)))) end

fun parse f =
    case TextIO.inputLine f of
        NONE => []
      | SOME line => (parseLine line) :: parse f

fun readData fname =
    let val f = TextIO.openIn fname
        val d = parse f
    in TextIO.closeIn f ; d end

(* First part *)

val start = (3, explode ".#...####")

fun rotate [a,b,c,d] =
    [[a,b,c,d],[b,d,a,c],[d,c,b,a],[c,a,d,b],
     [b,a,d,c],[a,c,b,d],[c,d,a,b],[d,b,c,a]]
  | rotate [a,b,c,d,e,f,g,h,i] =
    [[a,b,c,d,e,f,g,h,i],[c,f,i,b,e,h,a,d,g],[i,h,g,f,e,d,c,b,a],[g,d,a,h,e,b,i,f,c],
     [c,b,a,f,e,d,i,h,g],[a,d,g,b,e,h,c,f,i],[g,h,i,d,e,f,a,b,c],[i,f,c,h,e,b,g,d,a]]
  | rotate _ = raise Domain

fun update [] (_ : char list) = raise Fail "no applicable rule"
  | update ((k,v)::xs) square = if List.exists (fn r => square = r) (rotate k) then v
                                else update xs square

fun decomposeK (n, s) k =
    let val m = n div k
        (* Creates one sub-square *)
        fun g (di, dj) i j = if i = k then []
                             else if j = k then g (di, dj) (i + 1) 0
                             else List.nth (s, (dj + j) * n + di + i) :: g (di, dj) i (j + 1)
        (* Creates all sub-squares *)
        fun f i j = if j = m then []
                    else if i = m then f 0 (j + 1)
                    else g (i * k, j * k) 0 0 :: f (i + 1) j
    in f 0 0 end

fun decompose (n, s) = if n mod 2 = 0 then decomposeK (n, s) 2
                       else decomposeK (n, s) 3

fun isqrt n =
    if n < 0 then raise Domain
    else if n = 0 then 0
    else let fun f k last =
                 let val k' = (k * k + n) div (2 * k)
                 in if k' = k then k
                    else if k' = last then Int.min (k, last)
                    else f k' k
                 end
         in f 1 1 end

fun compose xs = let val m = isqrt (length xs)
                     val k = (isqrt o length o hd) xs
                     val n = m * k
                     fun f i j = if i = n then []
                                 else if j = n then f (i + 1) 0
                                 else let val xi = (j div k) * m + i div k
                                          val si = (j mod k) * k + (i mod k)
                                          val s = List.nth (xs, xi)
                                      in List.nth (s, si) :: f i (j + 1) end
                 in (n, f 0 0) end

fun iterate _ grid 0 = grid
  | iterate rules grid n = let val next = (compose o map (update rules) o decompose) grid
                           in iterate rules next (n - 1) end

fun pixelsOn grid =
    let fun f (c, n) = if c = #"#" then n + 1 else n
    in foldr f 0 grid end

fun adv21 () = pixelsOn (#2 (iterate (readData "adv21.txt") start 5))

(* Second part *)

(* Every third iteration is independent of its environment. *)

(* 3x3 => 3x3 list *)
fun update' rules grid = let val step1 = decompose (4, update rules grid)   (* 2x2 list *)
                             val step2 = compose (map (update rules) step1) (* 6x6 *)
                             val step3 = decompose step2                    (* 2x2 list *)
                         in map (update rules) step3 end                    (* 3x3 list *)

fun adv21b () = let val rules = readData "adv21.txt"
                    fun f 0 grid = pixelsOn grid
                      | f n grid = foldr op + 0 (map (f (n - 1)) (update' rules grid))
                in f 6 (#2 start) end
