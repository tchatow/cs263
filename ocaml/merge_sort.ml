


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


(* Comments on the difficulty of sorting with merge sort in ocaml: Splitting the list was something
that was very difficult for me to conceptualize in ocaml as I have to write my own recursive function to do that
I couldn't just keep track of where the middle of the list was and then sort it in place. I had to create a recursive
function that would iterate to the middle, and it had to return a pair of lists on every iteration.

I also had to get the idea of loops out of my head in order to write it in an Ocaml like fashion. 
Writing every loop by using a recursive function forced me into a different mode of thinking. I expected
that merge sort would be easier/more elegant with ocaml due to the functional/recursive nature of the language.
However, I think that it may be more elegant to write in languages that use loops more liberally.
*)
     