let validArguments = (Array.length Sys.argv) >= 2

let filePath = if validArguments then  Sys.argv.(1) else "./_input/day01.txt"

let wsTuple x = [" " ^ x] 
let  printTuples tuplesIn =
  let rec aux s =
    match s with
      | a :: b -> Printf.printf "%s" a; aux b
      | [] -> ()
  in
  match tuplesIn with
  | None -> ()
  | Some(s) -> aux s
    

let rec unzip stream currentList =
    try 
      let line = input_line stream
      in unzip stream ((wsTuple line) @ currentList)
    with
    | End_of_file -> Some(currentList)
    | _ ->
      close_in_noerr stream ;
      None 

let getTuples path = let file = open_in path in unzip file [""]

let printStringArr arg = 
  for i = 0 to Array.length arg -1 
      do 
         Printf.printf "[%i] %s \n" i arg.(i);
      done

let () = 
  print_endline "Entering Main";
  printStringArr Sys.argv;
  printTuples (getTuples filePath);
 
