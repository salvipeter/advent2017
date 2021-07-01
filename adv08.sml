fun parseLine line =
    let val lineNoLF = substring (line, 0, size line - 1)
        val words = String.tokens Char.isSpace lineNoLF
    in { register = hd words
       , increment = let val n = valOf (Int.fromString (List.nth (words, 2)))
                     in if (hd (tl words)) = "inc" then n else ~n end
       , condition = (List.nth (words, 4), List.nth (words, 5),
                      valOf (Int.fromString (List.nth (words, 6))))
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

fun getValue [] (_ : string) = 0
  | getValue ((x,v)::xs) y = if x = y then v else getValue xs y

fun incValue [] (y : string) i = [(y, i)]
  | incValue ((x,v)::xs) y i = if x = y then (x, v + i) :: xs
                               else (x, v) :: incValue xs y i

(* Uncomment this for the second part *)
(* fun incValue xs y i = (y, getValue xs y + i) :: xs *)

fun runInstruction {register, increment, condition} regs =
    let val (reg, rel, n) = condition
        val v = getValue regs reg
    in if case rel of
              "<"  => v <  n | ">"  => v >  n
            | "<=" => v <= n | ">=" => v >= n
            | "==" => v =  n | "!=" => v <> n
            | _ => raise Fail "unknown operator"
       then incValue regs register increment
       else regs
    end

fun run [] regs = regs
  | run (x::xs) regs = run xs (runInstruction x regs)

fun maxValue xs = foldr (fn ((_, n), a) => Int.max (n, a)) 0 xs

fun adv08 () = let val data = readData "adv08.txt"
               in maxValue (run data []) end
