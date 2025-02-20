
let map = List.map 
let identity x = x 
let p = print_endline
let pf = Printf.printf
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

let lToStr ( l:int list )= map ( string_of_int ) l |> List.fold_left ( ^ ) ""

let printList l = ( "|[Line]:"^(lToStr l) ^ "|") |> print_endline

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

type sign = 
  | Neg 
  | Zero 
  | Starting
  | Pos 
let dts x = if x = 0 then Zero else if x > 0 then Pos else Neg

let sod a b = dts (diff a b)
let des a b sign = sod a b |> ( = ) sign

let lineIsSafe2 linesIn = 
  let linesIn = linesIn |> intlist_of_str in
  let a :: b :: c :: rest = linesIn in 
  let defSign = 
      let ab = sod a b in
      let bc = sod b c in
      let ac = sod a c in 
  if ab = bc && bc == ac then ab
else if ab = ac && 
      


  let rec aux (skip:int) ( sign:sign ) list =

    match list with 
    | [] -> true
    | a :: [] -> true
    | a :: b :: rest -> 
      if skip = 0 then true 
      else if des a b sign then true else false 
    | a :: b :: c :: rest ->  
    let ab = sod a b in
      if sign <> ab  then
        
  


    
        



let linesAreSafe2 linesIn = map lineIsSafe2 linesIn



let part2 input = file |> lines |> identity

let () = p "Part 1:";p (part1 file);



  
