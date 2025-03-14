(*used chatgpt to create the generate_random_list function*)
 let generate_random_list n =
  let rec random_list_aux lst n =
    if n <= 0 then lst
    else random_list_aux (Random.int n :: lst) (n - 1)
  in
  random_list_aux [] n;;


let rec split_list_internal lst num acc =
  if acc = num then lst else
    let (left,right) = lst in
    split_list_internal ((List.hd right)::left, List.tl right) num (acc+1)

let split_list lst num = 
  let (lst1, lst2) = split_list_internal ([],lst) num 0 in
  (List.rev lst1, lst2)

let rec merge lst1 lst2 = 
  match lst1 with 
  | h1 :: t1 -> 
    (match lst2 with
    | h2 :: t2 -> if h2 > h1 then h1 :: merge t1 (lst2) else h2 :: merge (lst1) t2
    | [] -> lst1)
  | [] -> lst2

let rec merge_sort_aux lst len = 
  let left_len = len/2 in
  let right_len = (Int.to_float len)/.2.0 |> Float.ceil |> Float.to_int in
  match lst with 
  | [] -> []
  | [a] -> [a]
  | [a1; a2] -> if a1 > a2 then [a2;a1] else [a1;a2]
  | _ :: _ -> let (left, right) = split_list lst len in
  merge (merge_sort_aux left left_len) (merge_sort_aux right right_len) 


let merge_sort lst =
  merge_sort_aux lst (List.length lst)

(* let benchmark n =
  for i = 0 to n do
    let lst = generate_random_list (BatInt.pow 2 i) in
    let t1 = Core.Time_ns.now() in
    ignore(merge_sort lst);
    let t2 = Core.Time_ns.now() in
    print_int (List.length lst);
    print_string ", ";
    print_int (Core.Time_ns.to_int_ns_since_epoch t2 - Core.Time_ns.to_int_ns_since_epoch t1);
    print_endline "";
  done

let () = benchmark 24 *)




let rec benchmark k i = 
  if k <=0 then [] 
  else

    let lst = generate_random_list (BatInt.pow 2 i) in
    let t1 = Core.Time_ns.now() in
    ignore(merge_sort lst);
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
(* Comments on the difficulty of sorting with merge sort in ocaml: Splitting the list was something
that was very difficult for me to conceptualize in ocaml as I have to write my own recursive function to do that
I couldn't just keep track of where the middle of the list was and then sort it in place. I had to create a recursive
function that would iterate to the middle, and it had to return a pair of lists on every iteration.

I also had to get the idea of loops out of my head in order to write it in an Ocaml like fashion. 
Writing every loop by using a recursive function forced me into a different mode of thinking. I expected
that merge sort would be easier/more elegant with ocaml due to the functional/recursive nature of the language.
However, I think that it may be more elegant to write in languages that use loops more liberally.
*)

(* My original  split_list function looked like this: 

let rec split_list_internal lst num acc =
  if acc = num then lst else
    let (left,right) = lst in
    split_list_internal (left @ [List.hd right], List.tl right) num (acc+1)

let split_list lst num = 
  split_list_internal ([],lst) num 0
  
  However, after benchmarking the merge sort algorithm I realized that it was not running in
  O(nlogn), I compared it with the top down algorithm in ocaml for merge sort here: https://gist.github.com/mrvn/82577b32b83574048ce8
  and I realized that my split list algorithm was using the @ (append) operator which is O(n) time as opposed to the
  :: (cons) operator which is O(1) time according to this post https://stackoverflow.com/questions/40008949/time-complexity-of-and-ocaml
  *)
