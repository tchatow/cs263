
let rec bubble_sort lst =
  match lst with
  | [] -> []
  | [a] -> [a]
  | h1 :: t1 -> 
    match tail1 with
    | [] -> []
    | h2 :: t2 -> if h1 > h2 then h2 :: bubble_sort (h1 :: t2) else h1 :: bubble_sort (h2 :: t2)