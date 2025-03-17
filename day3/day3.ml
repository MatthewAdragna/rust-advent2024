  (* # Begin standard *)
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
let ip x= p x; x
let pf = Printf.printf
let pt_print x= p x; x
let entireFile f = In_channel.with_open_bin f In_channel.input_all |> String.trim
let linesSplit f= 
  let channel = In_channel.with_open_bin f In_channel.input_all
  in String.split_on_char '\n' channel 
  |> List.filter (fun x ->(String.length (String.trim x) ) >= 1) 

let printStrArr arrIn = 
  for i = 0 to (Array.length arrIn)-1 do
    Printf.printf "[%i :%s]" i arrIn.(i)
  done

let default_file = "input"

let file = if Array.length Sys.argv >=2  then Sys.argv.(1) else default_file

let elemFormat conversion intIn = 
  let strout = conversion intIn  
  in strout ^ ";"
let lToStr conversion  l=  let i = map ( elemFormat conversion) l |> List.fold_left (  ^  ) "[" in i ^ "]"
let printFunct conversion l = ( "|[Line]:"^(lToStr conversion l) ^ "|") |> print_endline
let printListSingle conversion l = l |> printFunct conversion
let printBoolListSingle l = ( printListSingle string_of_bool ) l; l
let printFlatListFunct conversion l = l |> flatten_tailed |> (printListSingle conversion) ; l
let printFlatIntList = (printFlatListFunct string_of_int)
let intlist_of_str strIn = String.split_on_char ' ' strIn |> map int_of_string 
  
let linesToIntList2 (l:  string list) = map intlist_of_str l
(* end standard *)

(* let parse_number str =  *)
(*   let rec form_string size acc str = *)
(*     match str with  *)
(*     | chr :: rest when size < 3 && char_is_digit chr ->  *)
(*        form_string (size + 1) ("" ^ acc) rest *)
(*     | chr :: _ when char_is chr "," && size < 3: *)
(*     | _ ->  *)
(*         if acc = "" then None else Some( int_of_string( acc )) *)
(*     in form_string 0 "" str *)
(**)

  let multStr str = identity str
  let regMult = Re.compile {|mult(\([0-9])\|}
  let findMultStrs str = 
  let part1 = entireFile |> findMultStrs
  let part1 = 1

  let () = p "Main:/n"; p "Part 1:"; p (string_of_int part1);
