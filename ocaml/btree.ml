open Core
type bnode = 
  BNone |
  BNode of {right: bnode; left: bnode}


(** The variants gave me some trouble at first. I had to understand how the 
 variants worked but after that it was easy to define a tree. They are like enums but even
 better **)

let rec init_tree depth =
  if depth = 0 then BNone else BNode {right=init_tree (depth-1); left=init_tree (depth-1)}

let rec node_count = function
  | BNode {right=r; left=l} -> 1+node_count r + node_count l
  | BNone -> 0


let rec benchmark k i = 
  if k <=0 then [] 
  else

    let t1 = Time_ns.now() in
    
    ignore(node_count (init_tree i));

    let t2 = Time_ns.now() in
    (* Stdlib.print_int (Time_ns.to_int_ns_since_epoch t2 - Time_ns.to_int_ns_since_epoch t1); *)
    (Time_ns.to_int_ns_since_epoch t2 - Time_ns.to_int_ns_since_epoch t1)::(benchmark (k-1) i)


let stats data =
    let n = Int.to_float (List.length data) in 
    let avg = ((Int.to_float (BatList.sum data)) /. n) in
    let stddev = ((List.fold_right data ~f:(fun num acc -> acc +. BatFloat.pow ((Int.to_float num)-.avg) 2.) ~init:0.) /. n) |> Float.sqrt in
    (avg, stddev)



let rec main i = 
  if i > 21 then () else
  let (avg, stddev) = stats (benchmark 10 i) in
  Stdlib.print_float avg;
  Stdlib.print_string "\t";
  Stdlib.print_float stddev;
  Stdlib.print_newline ();
  main (i +1)

let () = main 0