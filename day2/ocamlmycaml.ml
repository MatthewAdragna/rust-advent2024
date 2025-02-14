
let lines f= 
  let channel = In_channel.with_open_bin f In_channel.input_all
  in String.split_on_char '\n' channel 
  |> List.filter (fun x ->(String.length (String.trim x) ) >= 1) 
let printStrArr arrIn = 
  for i = 0 to (Array.length arrIn)-1 do
    Printf.printf "[%i :%s]" i arrIn.(i)
  done
let default_file = "/home/cata/GitHub/advent/day1/adventday1/source/input"
let file = if Array.length Sys.argv >=2  then Sys.argv.(1) else default_file



