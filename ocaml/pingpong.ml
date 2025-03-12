let i = ref 0
let cv = Condition.create ()
let mut = Mutex.create ()

let k_iters = 10000000

let t1 = Core.Time_ns.now ()

let th1 = Thread.create (fun _ ->
  (*Using a mutex with a condition var: https://ocaml.org/manual/5.3/api/Condition.html*) 
  
  Mutex.lock mut;
  while !i < k_iters do
    
    while not ((!i mod 2) = 0) do
      Condition.wait cv mut
    done;
    i := !i+1;
    Condition.broadcast cv;

  done;
  ) ()

let th2 = Thread.create(fun _ ->
    Mutex.lock mut;
    while !i < k_iters do
      while not ((!i mod 2) = 1) do
        Condition.wait cv mut
      done;
      i := !i+1;
      Condition.broadcast cv;

    done;
    Mutex.unlock mut;) ()


let () = Condition.broadcast cv

let () = Thread.join th1
let () = Thread.join th2

let t2 = Core.Time_ns.now ()

let () = Stdlib.print_int (Core.Time_ns.to_int_ns_since_epoch t2 - Core.Time_ns.to_int_ns_since_epoch t1)

