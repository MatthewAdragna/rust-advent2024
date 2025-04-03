
(* end standard *)
open AdventStd.Common
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
let test_case_2 =
  {|
...MMM....
...MAS....
...SSS....
..........
..........
..........
..........
..........
..........
..........
|}


let tests = 
  [
    (given_case,18);
    (
{|
XMAS
PPPP
PPPP
PPPP
|},1)
  ]

let validSequence = "XMAS"

(*Part 1 start*)
let wordSearch  strIn wordSearchTable= (*This function assumes that the word is NOT a palindrome - if it is you would have to divide the amount of matches by 2*)
  let strLen = String.length strIn in
  let wordsFound = ref 0 in
  let xpStr = explode strIn  in
  let xBound,yBound = getBounds wordSearchTable in
  for x = 0 to  xBound-1 do
    for y = 0 to  yBound-1 do
       Array.iter (fun dir-> match acc_list_in_dir dir strLen wordSearchTable (x,y) with 
        | None -> ()
        | Some(a) -> 
          if (  a =  xpStr ) then 
            wordsFound := !wordsFound + 1 ;() 
      ) dirList;
done
  done;
  !wordsFound
let singleListInDir table strLen x y dir= acc_list_in_dir dir strLen table (x,y) 
let getListByDir  wordSearchTable strLen x y= 
          Array.map ( singleListInDir wordSearchTable strLen x y ) dirList

let getAmtOfMatches xpStr  = 
              Array.fold_left (fun acc xS ->
                match xS with 
                  | None -> acc 
                  | Some(s) ->  if s = xpStr then acc + 1 else acc
              ) 0  

let wordSearchArr  strIn (wordSearchTable: char array array)= (*This function assumes that the word is NOT a palindrome - if it is you would have to divide the amount of matches by 2*)
  let strLen = String.length strIn in
  let xpStr = explode strIn in
  Array.mapi (
    fun x elem-> 
      Array.mapi (
        fun y _  -> 
           getListByDir wordSearchTable strLen x y |> getAmtOfMatches xpStr
      ) elem
  ) wordSearchTable
 
let tallySearched arrIn =
  Array.map (fun x -> Array.fold_left (+) 0 x) arrIn |> Array.fold_left (+) 0

let enumerateArr case = Array.mapi (fun i elem -> (i,elem) ) case ;;

let getListByDirEnum  wordSearchTable strLen x y= 
          Array.map (fun dir -> (dir, acc_list_in_dir dir strLen wordSearchTable (x,y) ) ) dirList

let checkByDir origStr tableGiven x y  =
   let exploded = explode origStr in 
  let strLen = String.length origStr in
    getListByDirEnum tableGiven strLen x y
  |> Array.map (
            fun elem ->
            let dir, opt = elem in
             let row = Option.get opt 
            in ( ( row = exploded ),dir,row )
   ) 

let testCases22 = List.map  (fun (x,_ ) -> wordSearchArr  validSequence (string_split_arr x )) tests
let currInput = file 4
let part1 = currInput |> linesSplitArr  |> wordSearchArr validSequence |> tallySearched
let () = print_int part1
(*Part 1 end*)
(* part 2 start *)
let mas = explode "MAS"

let inDir ( table : char array array) x y dir  =
  let d_dir = itr_dir dir (x,y) in
    match ( acc_list_in_dir (dirOpp dir) 3 table d_dir )
    with 
    | Some(a) when a = mas->true 
    | _ -> false


(* let isMasPlus table x y =  *)
(*   let aux = inDir table x y in *)
(*   let upDown = aux North || aux South in *)
(*   let sideBySide = aux East || aux West in *)
(*   if upDown && sideBySide then 1 else 0 *)



let isMasDiag table x y = 
  let aux = inDir table x y in
  let nw_se = aux NorthWest || aux SouthEast in
  let ne_sw = aux NorthEast || aux SouthWest in
  if nw_se && ne_sw then 1 else 0



let xmasSearchArr  (wordSearchTable: char array array) =
  Array.mapi (
    fun x elem-> 
      Array.mapi (
        fun y elem2 -> 
        if elem2 = 'A' then 
        ((isMasDiag wordSearchTable x y) )
       else 0
      ) elem
  ) wordSearchTable


let () = print_endline "\nPart 2:"
let part2Solver inp = inp  |> xmasSearchArr |> tallySearched 
let part2 =  currInput |> linesSplitArr |> part2Solver
let () = print_int part2

let () = print_endline "\n test for part 2:\n"
let sptc2 = string_split_arr test_case_2
let testShow = sptc2 |> xmasSearchArr
let tested = testShow |> tallySearched 

let showEnum = enumerateArr sptc2
let enumShow = enumerateArr testShow
(* part 2 end *)











