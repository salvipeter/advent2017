val nsteps = 324

(* First part *)

fun spinlock buffer 2017 = Array.sub (buffer, 2017)
  | spinlock buffer p =
    let fun step k 0 = k
          | step k s = step (Array.sub (buffer, k)) (s - 1)
        val next = step p nsteps
        val old = Array.sub (buffer, next)
    in Array.update (buffer, next, p + 1)
     ; Array.update (buffer, p + 1, old)
     ; spinlock buffer (p + 1)
    end

fun adv17 () = spinlock (Array.array (2018, 0)) 0

(* Second part *)

(* Using the solution of the first part ~ 2.5m with MLton *)

fun spinlock' p 50000001 result = result
  | spinlock' p n result =
    let val next = (p + nsteps) mod n + 1
    in spinlock' next (n + 1) (if next = 1 then n else result) end

fun adv17b () = spinlock' 0 1 0
