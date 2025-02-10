
let validArguments = Array.length Sys.argv >= 2
let filePath = if validArguments then  Sys.argv.(1) else "input" 

let performLineFunction x = Printf.printf "%s \n" x 
let () = 
  for i = 0 to Array.length Sys.argv -1 do 
      Printf.printf "[%i] %s \n" i Sys.argv.(i)
  done;
  
  let file = open_in filePath in 
  try
    while true do
    let line = input_line file in
    performLineFunction line;
    done
  with 
  | End_of_file -> close_in file
  | e -> close_in_noerr file; raise e


Printf.printf "%s" "Finished"


















