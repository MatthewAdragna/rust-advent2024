let printStrArr arrIn = 
  for i = 0 to (Array.length arrIn)-1 do
    Printf.printf "[%i :%s]" i arrIn.(i)
  done
let default_file = ".../../../source/input"
let file = if Array.length Sys.argv >=2  then Sys.argv.(1) else default_file

let lines f= 
  let channel = In_channel.with_open_bin f In_channel.input_all
  in String.split_on_char '\n' channel 
  |> List.filter (fun x ->(String.length (String.trim x) ) >= 1) 


let getTuple (line: string) =
  let lineIn = String.split_on_char ' ' line in 
    match lineIn with
  | start :: rest  ->
      let rec findLast lineIn =
        match lineIn with
      | [x] ->int_of_string x
        | _:: rest -> findLast rest
        | [] -> -1
      in 
    ((int_of_string start), (findLast rest))
  | [] -> failwith "Improper String to tupleizeizeieize" 
  
  
let getLists lines = 
    let rec aux (linesLeft: string list) tupled:(int list * int list) = 
      match linesLeft with
  | [line] -> let a,b = getTuple line in let tA,tB = tupled in ((tA @ [a]),( tB @ [b]))
  | line :: rest -> let a,b = getTuple line in let tA,tB = tupled in aux rest ((tA @ [a]), (tB @ [b]))
  | [] -> tupled
  in
    aux lines ([],[])


let ()= 
  print_endline (Sys.getcwd ());
  printStrArr Sys.argv;
  Printf.printf "\n The input file is %s \n" file;

  let left,right = getLists (lines file) in 
  let left, right = (List.sort compare left, List.sort compare right) in
  let summed = List.fold_left (+) 0 (List.map2  ( fun x y -> abs ( (-) x y))  left right)
  in
  print_endline ("Problem 1:" ^(string_of_int summed))



