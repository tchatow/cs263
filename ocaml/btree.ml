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


let benchmark = 
  let t1 = Time_ns.now() in
    for i = 0 to 21 do
      ignore((node_count (init_tree i)))
    done;
    let t2 = Time_ns.now() in
    print_int (Time_ns.to_int_ns_since_epoch t2 - Time_ns.to_int_ns_since_epoch t1);
