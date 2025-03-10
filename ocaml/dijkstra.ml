(* #require "csv";; *)

module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)

type edge = {to_node: int; len: float}

type nodel = {edges: edge list}

let empty_graph : nodel IntMap.t = IntMap.empty

let csvfile = Csv.load "NewYork_Edgelist.csv"

let rec take_first_n lst n  =
  if n = 0 then [] else List.hd lst :: take_first_n (List.tl lst) (n-1) 


let add_edge (graph: nodel IntMap.t) (record: string list) =
  (* let () = print_string (List.nth record 2) in *)
  let start_node = int_of_string (List.nth record 2) in
  let end_node = int_of_string (List.nth record 3) in
  let distance = float_of_string (List.nth record 5) in

  let graph1 = 
    if IntMap.mem start_node graph 
      then IntMap.update start_node (Option.map (fun value -> {edges = value.edges @[{to_node=end_node; len=distance}]})) graph
    else IntMap.add start_node {edges=[{to_node=end_node; len=distance}]} graph in
    
    if IntMap.mem end_node graph1 
      then IntMap.update end_node (Option.map (fun value -> {edges = value.edges @[{to_node=start_node; len=distance}]})) graph1
    else IntMap.add end_node {edges=[{to_node=start_node; len=distance}]} graph1
    
(* It is wieldy to update a record, any easier way to do this? *)

(* Only fold on 10000 elements*)

let firstn n = take_first_n (List.tl csvfile) n
let make_graph n = List.fold_left add_edge empty_graph (firstn n)


let find_min_from_set unvisited distance =
  IntSet.fold (fun curr_node smallest_node -> 
    if ((IntMap.find curr_node distance) < (IntMap.find smallest_node distance)) then curr_node else smallest_node)
    unvisited
    (IntSet.min_elt unvisited)

let rec dijkstra_inner (graph: nodel IntMap.t) start_n end_n unvisited distance prev =
  (* if it's empty return distance *)
  if IntSet.is_empty unvisited then (distance, prev) 
  else 
    (* If we have min distance to end node or all are unreachable then return *)
    let min_node = find_min_from_set unvisited distance in
    (* if min_node == end_n || (IntMap.find min_node distance) = infinity then distance 
    else *)
    let min_node_dist = IntMap.find min_node distance in

    let update_step = 
      (fun (distance_map, prev) edge -> 
      let curr_dist = IntMap.find edge.to_node distance_map in
      if not (IntSet.mem edge.to_node unvisited) then (distance_map, prev) 
      else if (min_node_dist +. edge.len) < curr_dist then 
        (IntMap.add edge.to_node (min_node_dist +. edge.len) distance_map, IntMap.add edge.to_node min_node prev)
      else  (distance_map, prev)) in
    
    let dist_and_prev = (distance, prev) in
    
    let (updated_distance, updated_prev) = (IntMap.find min_node graph).edges |> 
    List.fold_left update_step dist_and_prev in 
    dijkstra_inner graph start_n end_n (IntSet.remove min_node unvisited) updated_distance updated_prev



let rec print_path path_map start_n end_n =
  print_int end_n;
  print_endline "";
  if start_n = end_n then ()
  else 
    let next = IntMap.find end_n path_map in
    print_path path_map start_n next

let dijkstra (graph : nodel IntMap.t) start_n end_n  = 
  let unvisited = (IntMap.bindings graph) |> 
    List.fold_left (fun set (node_name, _) -> IntSet.add node_name set) IntSet.empty in
  
  let distance = (
    (IntMap.bindings graph) |> 
    List.fold_left (fun map (node_name, _) -> IntMap.add node_name infinity map) IntMap.empty) 
    |> IntMap.add start_n 0.  in
  
  ignore(dijkstra_inner graph start_n end_n unvisited distance IntMap.empty)
  (* print_path path_map start_n end_n *)

  


(* let () = dijkstra graph 1 4 *)
  
  

let rec benchmark k i = 
  if k <=0 then [] 
  else
    let num_nodes = (BatInt.pow 2 i) in
    let graph = make_graph num_nodes in
    let t1 = Core.Time_ns.now() in

    for i = 0 to 999 do
      let end_n = Random.int num_nodes +1 in
      let start_n = Random.int num_nodes +1 in 
      ignore(try dijkstra graph start_n end_n with Not_found -> ());
    done;

    let t2 = Core.Time_ns.now() in
    (* Stdlib.print_int (Time_ns.to_int_ns_since_epoch t2 - Time_ns.to_int_ns_since_epoch t1); *)
    (Core.Time_ns.to_int_ns_since_epoch t2 - Core.Time_ns.to_int_ns_since_epoch t1)::(benchmark (k-1) i)


let stats data =
    let n = Int.to_float (List.length data) in 
    let avg = ((Int.to_float (BatList.sum data)) /. n) in
    let stddev = ((Core.List.fold_right data ~f:(fun num acc -> acc +. BatFloat.pow ((Int.to_float num)-.avg) 2.) ~init:0.) /. n) |> Float.sqrt in
    (avg, stddev)



let rec main i = 
  if i > 24 then () else
  let (avg, stddev) = stats (benchmark 10 i) in
  Stdlib.print_float avg;
  Stdlib.print_string "\t";
  Stdlib.print_float stddev;
  Stdlib.print_newline ();
  main (i +1)

let () = main 0;




  

(* let () = print_int (IntMap.cardinal graph) *)





(*Had to understand what fold_left actually does, originally wanted to use Csv.iter 
 but this returns a unit type so it is only useful for printing, in this case I want to return a 
 graph from the function *)



