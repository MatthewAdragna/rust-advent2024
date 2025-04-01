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
let getFirstDigit str =
  match str with 
  | a  :: ',' :: rest when (charListAreDigits [a]) -> ( int_of_string_opt (implode [a]) , rest )
  | a :: b :: ',' :: rest when (charListAreDigits [a;b]) -> ( int_of_string_opt (implode [a;b]) , rest )
  | a :: b :: c :: ',' :: rest  when (charListAreDigits [a;b;c])-> ( int_of_string_opt (implode [a;b;c]) , rest )
  | _ -> (None, str)
let getLastDigit str =
  match str with 
  | a  :: ')' :: rest when (charListAreDigits [a]) -> ( int_of_string_opt (implode [a]), rest )
  | a :: b :: ')' :: rest when (charListAreDigits [a;b]) -> ( int_of_string_opt (implode [a;b]) , rest )
  | a :: b :: c :: ')' :: rest when (charListAreDigits [a;b;c]) -> ( int_of_string_opt (implode [a;b;c]) , rest )
  | _ -> (None, str)
let sti = string_of_int
let genPair x y =("("^ sti x   ^ "," ^  sti y  ^ ")") 
let pPair x y = p (genPair x y)     
let findMultStrs str = 
  let rec findCandidates acc strLeft =
    match strLeft with 
    | [] -> acc
    | 'm'::'u'::'l'::'('::rest -> 
      ( match (getFirstDigit rest) with
        | (Some(givenFirst), rest) -> 
          (match getLastDigit rest with
            | (Some(givenLast), rest) -> 
              findCandidates ( acc @ [givenLast,givenFirst] ) rest
            | (None,rest) -> findCandidates acc rest) 
        | (_,rest) -> findCandidates acc rest ) 
    | _ :: rest -> findCandidates acc rest
  in findCandidates []  str 
let testCase = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
let fullFileStr = file |> entireFile 
let testLists =
  [
    ( "mul(4*",0 );
    ( "mul(6,9!",0 );
    ( "?(12,34)",0 );
    ( "mul ( 2 , 4 )",0 );
    ( testCase,161 );
  ]
let calcTotal listOfPairs = listOfPairs |> List.fold_left (fun acc (x,y)-> acc + (x * y) ) 0 
  let runTest x = let expr, expected = x in let a = (( calcTotal (findMultStrs expr) ) = expected ) in let () = assert a in a
  let runTests  = map runTest (map (fun (x,y) -> (explode x, y)) testLists)
  let part1 stringIn = stringIn |> findMultStrs
  let fileParsed = fullFileStr |> explode |> part1
let () = p "Main:"; p "Part 1:"; p (string_of_int ( fileParsed |> calcTotal));;


  let part2 stringIn =
      let rec tokenize accString accPairs doState parsing = 
      (
        match parsing with 
        | 'd' :: 'o' :: '(' :: ')' :: rest when doState = false -> tokenize accString accPairs true rest 
        | 'd' :: 'o' ::'n'::'\'' :: 't' :: '(' :: ')' :: rest  when doState = true -> 
            let newPairs = findMultStrs accString in
              tokenize [] ( accPairs @ newPairs ) false rest 
        | someChar :: rest -> 
            if doState = false then tokenize accString accPairs doState rest
            else tokenize (accString @ [ someChar ]) accPairs doState rest
        | [] -> 
            let newPairs = findMultStrs accString in 
              (accPairs @ newPairs) 
      )
      in tokenize [] [] true stringIn


let fileParsed2 = fullFileStr |> explode |> part2
let p1L = List.length fileParsed
let p2L = List.length fileParsed2
let () = p "Main:"; p "Part 2:"; p (string_of_int ( fileParsed2 |> calcTotal));;
