val steps = 12919244

datatype state = A | B | C | D | E | F

fun run tape _ 0 = tape
  | run (_, []) _ _ = raise Fail "invalid tape"
  | run (rleft, x :: right) state n =
    let fun goLeft y s = if rleft = [] then run ([], 0 :: y :: right) s (n - 1)
                         else run (tl rleft, hd rleft :: y :: right) s (n - 1)
        fun goRight y s = if right = [] then run (y :: rleft, [0]) s (n - 1)
                          else run (y :: rleft, right) s (n - 1)
    in case state of
        A => if x = 0 then goRight 1 B else goLeft  0 C
      | B => if x = 0 then goLeft  1 A else goRight 1 D
      | C => if x = 0 then goRight 1 A else goLeft  0 E
      | D => if x = 0 then goRight 1 A else goRight 0 B
      | E => if x = 0 then goLeft  1 F else goLeft  1 C
      | F => if x = 0 then goRight 1 D else goRight 1 A
    end

fun adv25 () = let val (rleft, right) = run ([], [0]) A steps
               in foldr op + 0 (rleft @ right) end
