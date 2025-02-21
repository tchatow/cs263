

let rec bubble lst =
  match lst with
  | [] -> []
  | [a] -> [a]
  | h1 :: t1 -> 
    match t1 with
    | [] -> []
    | h2 :: t2 -> if h1 > h2 then h2 :: bubble (h1 :: t2) else h1 :: bubble (h2 :: t2)

let rec is_sorted lst = 
  match lst with 
  | [] -> true
  | [a] -> true
  | h :: t -> if h > (List.hd t) then false else is_sorted t


let rec bubble_sort lst = 
  if is_sorted lst then lst else bubble_sort (bubble lst)
(* Big realization for bubble sort is that I had to make two functions, a bubble utility function
 and then a bubble sort function  that uses the bubble utility function*)