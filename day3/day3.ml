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
let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;
let implode l =  String.concat "" (map ( String.make 1 ) l )
 let getFirstDigit str =
    match str with 
    | a :: b :: c :: ',' :: rest -> ( int_of_string_opt (implode [a;b;c]) , rest )
    | a :: b :: ',' :: rest -> ( int_of_string_opt (implode [a;b]) , rest )
    | a  :: ',' :: rest -> ( int_of_string_opt (implode [a]) , rest )
    | _ -> (None, str)
let getLastDigit str =
    match str with 
    | a :: b :: c :: ')' :: rest -> ( int_of_string_opt (implode [a;b;c]) , rest )
    | a :: b :: ')' :: rest -> ( int_of_string_opt (implode [a;b]) , rest )
    | a  :: ')' :: rest -> ( int_of_string_opt (implode [a]), rest )
    | _ -> (None, str)

    
    
  let multStr str = identity str
  let findMultStrs str = let expl = explode str in 
    let rec findCandidates acc strLeft =
      match strLeft with 
      | [] -> acc
      | 'm'::'u'::'l'::'('::rest -> 
            ( match (getFirstDigit rest) with
                | (Some(givenFirst), rest) -> 
                    (match getLastDigit rest with
                    | (Some(givenLast), rest) ->  findCandidates ( acc + (givenLast * givenFirst) ) rest
                    | (None,rest) -> findCandidates acc rest) 
                | (_,rest) -> findCandidates acc rest ) 
      | arb :: rest -> findCandidates acc rest
    in findCandidates 0 expl

           
  let part1 = file |> entireFile |> findMultStrs |> print_int
  let part1 = 1

  let () = p "Main:/n"; p "Part 1:"; p (string_of_int part1);
