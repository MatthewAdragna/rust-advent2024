
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
let charListAreDigits chrList = chrList |> map (fun x -> 
  match x with 
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false
)  
  |> List.fold_left (&&) true
let explode s =
  let rec expl i l =
    if i < 0 then l else
      expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;
let implode l =  String.concat "" (map ( String.make 1 ) l )
(* end standard *)

let given_case =
  {|
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
|}

let testCases = [

  (given_case,18);
] 




