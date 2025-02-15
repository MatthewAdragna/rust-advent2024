
let lines f= 
  let channel = In_channel.with_open_bin f In_channel.input_all
  in String.split_on_char '\n' channel 
  |> List.filter (fun x ->(String.length (String.trim x) ) >= 1) 
let printStrArr arrIn = 
  for i = 0 to (Array.length arrIn)-1 do
    Printf.printf "[%i :%s]" i arrIn.(i)
  done
let default_file = "input"
let file = if Array.length Sys.argv >=2  then Sys.argv.(1) else default_file


let lineToIntList l = let lineList = String.split_on_char ' ' l in
  let rec aux listOut lineList =
    match lineList with
    | x :: rest -> if x = "" then aux ([(int_of_string x)] @ listOut) rest else aux listOut rest
    | [] -> listOut 
    in aux [] lineList
  
let linesToIntIntList l = 
  let rec aux totalList l =
    match l with
      | [] -> totalList
      | s :: rest -> aux ((lineToIntList s) @ totalList) rest
  in aux [] l
let rec badAdventure listIn =
   match listIn with
  |a :: b :: rest -> 
  let diff = a - b in
    if diff > 0 || (abs diff) > 2 then 0 else badAdventure ([b] @  rest)
  | _  -> 1 


let rec goodAdventure listIn =
   match listIn with
  |a :: b :: rest -> 
  let diff = a - b in
    if diff < 0 || (abs diff) > 2 then 0 else goodAdventure ([b] @  rest)
  | _  -> 1 


let safetyDance x =  
  let a,b,rest = match x with
  |a :: b :: rest -> (a,b,rest)
  |_ -> (-1,-1,[-1])

  in
  let diff = a - b in 
  if abs diff > 2  then
    0
  else if diff < 0 then
    badAdventure ([b] @ rest) 
  else
    goodAdventure ([b] @ rest)

let getSafety l =
  let rec aux lIn lOut = 
    match lIn with 
    | x :: rest -> aux (rest) ([(safetyDance x)] @ lOut)
    | [] -> lOut
  in 
    aux l []

 




let () = 
  let linesIn = lines file in
  
  let safeLevels = List.fold_left (+) 0 (getSafety (linesIn)) in in print_endline (string_of_int safeLevels);
