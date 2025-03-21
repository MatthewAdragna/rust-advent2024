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
