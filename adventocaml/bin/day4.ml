
(* end standard *)
open AdventStd
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
  let xpStr = explode strIn |> List.rev in
  let xBound,yBound = getBounds wordSearchTable in
  for x = 0 to  xBound-1 do
    for y = 0 to  yBound-1 do
      Array.iter (fun dir-> match acc_list_in_dir dir strLen wordSearchTable (x,y) 
      with 
      | None -> ()
        | Some(a) -> 
          if ( matchShallowLists a xpStr ) then 
            wordsFound := !wordsFound + 1 ;() 
      ) dirList;
done
  done;
  !wordsFound

let testCases = List.map  (fun (x,_ ) -> wordSearch  validSequence (string_split_arr x )) tests
let currInput = file 4
let part1 = currInput |> linesSplitArr  |> wordSearch validSequence 
let () = print_int part1
(*Part 1 end*)











let testCases = [
  (given_case,18);
] 







