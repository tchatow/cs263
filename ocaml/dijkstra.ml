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

let first10000 = take_first_n (List.tl csvfile) 10000
let graph = List.fold_left add_edge empty_graph first10000


let dijkstra_inner (graph: nodel IntMap.t) start_n end_n unvisited distance =
  let smallest = IntSet.fold

let dijkstra (graph : nodel IntMap.t) start_n end_n  = 
  let unvisited = (IntMap.bindings graph) |> 
    List.fold_left (fun set (node_name, _) -> IntSet.add node_name set) IntSet.empty in
  
  let distance = (
    (IntMap.bindings graph) |> 
    List.fold_left (fun map (node_name, _) -> IntMap.add node_name infinity map) IntMap.empty) 
    |> IntMap.add start_n 0.  in
  
  




  

let () = print_int (IntMap.cardinal graph)





(*Had to understand what fold_left actually does, originally wanted to use Csv.iter 
 but this returns a unit type so it is only useful for printing, in this case I want to return a 
 graph from the function *)



