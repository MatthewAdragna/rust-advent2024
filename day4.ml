
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

let getListByDir  wordSearchTable strLen y x= 
          Array.map (fun dir -> acc_list_in_dir dir strLen wordSearchTable (x,y) ) dirList

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
           getListByDir wordSearchTable strLen y x |> getAmtOfMatches xpStr
      ) elem
  ) wordSearchTable
 
let tallySearched arrIn =
  Array.map (fun x -> Array.fold_left (+) 0 x) arrIn |> Array.fold_left (+) 0

let print2d case = Array.map (Array.map (fun x -> string_of_int x) ) (List.nth case 0);;


let testCases22 = List.map  (fun (x,_ ) -> wordSearchArr  validSequence (string_split_arr x )) tests
let currInput = file 4
let part1 = currInput |> linesSplitArr  |> wordSearchArr validSequence |> tallySearched
let () = print_int part1
(*Part 1 end*)











let testCases = [
  (given_case,18);
] 







