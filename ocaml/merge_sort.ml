


let rec split_list_internal lst num acc =
  let () = assert ((List.length (List.nth lst 1)) > num - acc) in
  if acc = num then lst else
    match lst with
    | [] -> raise (Invalid_argument "list should not be empty")
    | h :: t -> split_list_internal [h @ [(List.hd (List.hd t))]; (t |> List.hd |> List.tl)] num (acc+1)

let split_list lst num = 
  split_list_internal [[];lst] num 0

let rec merge lst1 lst2 = 
  match lst1 with 
  | h1 :: t1 -> 
    (match lst2 with
    | h2 :: t2 -> if h2 > h1 then [h1] @ merge t1 ([h2] @ t2) else [h2] @ merge ([h1]@t1) t2
    | [] -> lst1)
  | [] -> lst2

let rec merge_sort lst = 
  match lst with 
  | [] -> []
  | [a] -> [a]
  | [a1; a2] -> if a1 > a2 then [a2;a1] else [a1;a2]
  | _ :: _ -> let split = split_list lst ((List.length lst)/2) in
  merge (merge_sort (List.nth split 0)) (merge_sort (List.nth split 1)) 



     
