
let generate_random_list n =
  let rec random_list_aux lst n =
    if n <= 0 then lst
    else random_list_aux (Random.int n :: lst) (n - 1)
  in
  random_list_aux [] n;;

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

  let rec benchmark k i = 
    if k <=0 then [] 
    else
  
      let lst = generate_random_list (BatInt.pow 2 i) in
      let t1 = Core.Time_ns.now() in
      ignore(bubble_sort lst);
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
(* Big realization for bubble sort is that I had to make two functions, a bubble utility function
 and then a bubble sort function  that uses the bubble utility function*)