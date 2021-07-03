datatype regval = Reg of char | Val of int

datatype instr = Snd of regval
               | Set of regval * regval
               | Add of regval * regval
               | Mul of regval * regval
               | Mod of regval * regval
               | Rcv of regval
               | Jgz of regval * regval

fun parseLine line =
    let val lineNoLF = substring (line, 0, size line - 1)
        val words = String.tokens Char.isSpace lineNoLF
        fun makeRegVal str = case Int.fromString str of
                                 NONE => Reg (String.sub (str, 0))
                               | SOME n => Val n
        val a = makeRegVal (hd (tl words))
        val b = makeRegVal (hd (tl (tl words))) handle Empty => Val 0
    in case hd words of
           "snd" => Snd a
         | "set" => Set (a, b)
         | "add" => Add (a, b)
         | "mul" => Mul (a, b)
         | "mod" => Mod (a, b)
         | "rcv" => Rcv a
         | "jgz" => Jgz (a, b)
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
    let val regs = Array.array (16, 0)
        fun get (Val n) = n 
          | get (Reg c) = Array.sub (regs, ord c - ord #"a")
        fun set (Reg c) n = Array.update (regs, ord c - ord #"a", n)
          | set (Val _) _ = raise Fail "invalid operation"
        fun f ip sound =
            let val instr = Array.sub (program, ip)
            in case instr of
                   Snd a => f (ip + 1) (get a)
                 | Set (a, b) => ( set a (get b)
                                 ; f (ip + 1) sound )
                 | Add (a, b) => ( set a (get a + get b)
                                 ; f (ip + 1) sound )
                 | Mul (a, b) => ( set a (get a * get b)
                                 ; f (ip + 1) sound )
                 | Mod (a, b) => ( set a (get a mod get b)
                                 ; f (ip + 1) sound )
                 | Rcv a => if get a <> 0 then sound
                            else f (ip + 1) sound
                 | Jgz (a, b) => if get a > 0 then f (ip + get b) sound
                                 else f (ip + 1) sound
            end
    in f 0 0 end

fun adv18 () = run (Array.fromList (readData "adv18.txt"))

(* Second part *)

fun run program =
    let fun f regs ip input output =
            let val instr = Array.sub (program, ip)
                fun get (Val n) = n 
                  | get (Reg c) = Array.sub (regs, ord c - ord #"a")
                fun set (Reg c) n = Array.update (regs, ord c - ord #"a", n)
                  | set (Val _) _ = raise Fail "invalid operation"
            in case instr of
                   Snd a => f regs (ip + 1) input (get a :: output) (* reverse order *)
                 | Set (a, b) => ( set a (get b)
                                 ; f regs (ip + 1) input output )
                 | Add (a, b) => ( set a (get a + get b)
                                 ; f regs (ip + 1) input output )
                 | Mul (a, b) => ( set a (get a * get b)
                                 ; f regs (ip + 1) input output )
                 | Mod (a, b) => ( set a (get a mod get b)
                                 ; f regs (ip + 1) input output )
                 | Rcv a => if input = [] then (regs, ip, output)
                            else ( set a (hd input)
                                 ; f regs (ip + 1) (tl input) output )
                 | Jgz (a, b) => let val next = if get a > 0 then ip + get b else ip + 1
                                 in f regs next input output end
            end
        val result = ref 0
        (* Run first program until rcv, then switch with the second one *)
        (* Record number of outputs when the first program is "program 1" *)
        fun coordinate (regs, ip, output) state1 input p1 =
            (* Check for deadlock, but only when there already was communication *)
            if input = [] andalso !result > 0 then !result
            else let val nextState = f regs ip input []
                     val output' = rev (#3 nextState)
                 in if p1 then result := !result + length output' else ()
                  ; coordinate state1 nextState output' (not p1)
                 end
        val regs0 = Array.array (16, 0)
        val regs1 = Array.array (16, 0)
    in Array.update (regs1, 15, 1)
     ; coordinate (regs0, 0, []) (regs1, 0, []) [] false
    end

fun adv18b () = run (Array.fromList (readData "adv18.txt"))
