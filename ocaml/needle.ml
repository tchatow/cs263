
let search = Sys.argv.(2)

let chan = open_in Sys.argv.(1)
let data =  really_input_string chan (in_channel_length chan)

let rec find_all str start =
  let next = (BatString.find_from data start str) in
  print_int next;
  print_endline "";
  find_all str (next+1);;


try find_all search 0 with Not_found -> () 

  