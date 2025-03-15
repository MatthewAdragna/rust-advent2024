
let flatten_tailed list_in =
  let rec aux l acc =
  match l with 
    | [] -> acc 
    | elem :: rest -> aux rest ( acc @ elem) 
  in 
  aux list_in []
let map = List.map 
let identity x = x 
let p = print_endline
let pf = Printf.printf
let pt_print x= p x; x
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

let elemFormat intIn = 
  let strout = string_of_int intIn  
  in strout ^ ";"
let lToStr ( l:int list )=  let i = map ( elemFormat ) l |> List.fold_left (  ^  ) "[" in i ^ "]"

let printList l = ( "|[Line]:"^(lToStr l) ^ "|") |> print_endline
let printListSingle l = l |> printList; l
let pt_printList l = l |> flatten_tailed |> printList ; l
let intlist_of_str strIn = String.split_on_char ' ' strIn |> map int_of_string 
  
let linesToIntList2 (l:  string list) = map intlist_of_str l

let diff = fun x y -> ( - ) x y
let diffTup (x,y) = diff x y

let rec pairs listIn =
    match listIn with 
    | a :: (b :: _ as rest) ->  (a,b) :: pairs rest
    | _ -> []
let diffs l = pairs l |> map diffTup 
let is_dec listIn = List.for_all (fun x -> x < 0) listIn 
let is_inc listIn = List.for_all (fun x -> x > 0) listIn 
let is_bounded  listIn = List.for_all(fun x -> let ab = abs x in ab > 0 && ab < 4) listIn
let safe listIn = is_bounded listIn && (is_inc listIn || is_dec listIn)
let lineIsSafe1 lineIn = intlist_of_str lineIn |> diffs |> safe
let linesAreSafe1 linesIn = map lineIsSafe1 linesIn
let countLines listIn = map (fun x -> match x with | true -> 1 | false -> 0) listIn |> List.fold_left ( + ) 0
let part1  input = file |> lines |> linesAreSafe1 |> countLines |> string_of_int


let rec remove_nth_from_list n ( listIn:int list ) =
  let rec aux acc n listIn = 
      match listIn with 
  | [] -> []
  | _ :: rest when n <= 0 -> acc @ rest 
  | curr :: rest -> aux ([ curr ] @ acc) ((-) n 1 ) rest
  in
    aux [] n listIn

  (*   match listIn with *)
  (* |   []-> [0;1]  *)
  (*     |  _ :: rest when n = 0 -> rest *)
  (*     |  a :: rest -> a::(remove_nth_from_list (n-1) rest) *)

let getLinePermutations lineIn= 
  (* let () =p "OG:" ;printList lineIn in *)
  lineIn :: List.mapi (fun i _ -> ( remove_nth_from_list i lineIn ) ) lineIn

let lineIsSafe2 lineIn = lineIn |> intlist_of_str |> getLinePermutations  |> map( map string_of_int )|> map linesAreSafe1  
(* let linesAreSafe2 linesIn = map lineIsSafe2 linesIn |> map countLines |> List.fold_left ( + ) 0 *)

(* let part2 input = file |> lines |> linesAreSafe2 |> string_of_int  *)

let () = p "Part 1:";p (part1 file);
         (* p "Part 2:";p (part2 file); *)

  
