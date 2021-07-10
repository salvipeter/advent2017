datatype regval = Reg of char | Val of int

datatype instr = Set of regval * regval
               | Sub of regval * regval
               | Mul of regval * regval
               | Jnz of regval * regval

fun parseLine line =
    let val lineNoLF = substring (line, 0, size line - 1)
        val words = String.tokens Char.isSpace lineNoLF
        fun makeRegVal str = case Int.fromString str of
                                 NONE => Reg (String.sub (str, 0))
                               | SOME n => Val n
        val a = makeRegVal (hd (tl words))
        val b = makeRegVal (hd (tl (tl words))) handle Empty => Val 0
    in case hd words of
           "set" => Set (a, b)
         | "sub" => Sub (a, b)
         | "mul" => Mul (a, b)
         | "jnz" => Jnz (a, b)
         | _  => raise Fail "invalid instruction"
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

fun run program =
    let val regs = Array.array (8, 0)
        fun get (Val n) = n 
          | get (Reg c) = Array.sub (regs, ord c - ord #"a")
        fun set (Reg c) n = Array.update (regs, ord c - ord #"a", n)
          | set (Val _) _ = raise Fail "invalid operation"
        fun f ip muls =
            if ip >= Array.length program then muls
            else let val instr = Array.sub (program, ip)
                 in case instr of
                        Set (a, b) => ( set a (get b)
                                      ; f (ip + 1) muls )
                      | Sub (a, b) => ( set a (get a - get b)
                                      ; f (ip + 1) muls )
                      | Mul (a, b) => ( set a (get a * get b)
                                      ; f (ip + 1) (muls + 1) )
                      | Jnz (a, b) => if get a <> 0 then f (ip + get b) muls
                                      else f (ip + 1) muls
                 end
    in f 0 0 end

fun adv23 () = run (Array.fromList (readData "adv23.txt"))

(* Second part *)

(* Assumes n > 1 *)
fun isPrime n = let fun f k = k * k > n orelse n mod k <> 0 andalso f (k + 2)
                in n mod 2 <> 0 andalso f 3 end

fun adv23b () = let val start = 81 * 100 + 100000
                    val stop = start + 17000
                    fun f n = if n > stop then 0
                              else if isPrime n then f (n + 17)
                              else 1 + f (n + 17)
                in f start end
