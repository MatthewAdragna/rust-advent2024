module Common = struct
(* # Begin standard *)
    let flatten_tailed (list_in:'a list list )=
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

    let get_default_file x= Printf.sprintf "_input/day%02d.txt" x

    let file num = if Array.length Sys.argv >=2  then Sys.argv.(1) else (get_default_file num)
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
    let charIsDigit charIn =   match charIn with 
      | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
      | _ -> false
    let charListAreDigits chrList = chrList |> map charIsDigit |> List.fold_left (&&) true
    let explode s =
      let rec expl i l =
        if i < 0 then l else
          expl (i - 1) (s.[i] :: l) in
      expl (String.length s - 1) [];;
    let implode l =  String.concat "" (map ( String.make 1 ) l )
    (* Start : Functions Added Since*)
    let rec list_get_index_single ind arr = if ind < 0 then None else 
        match arr with 
        | elem :: rest ->
          if ind = 0 then Some (elem)
          else list_get_index_single (( - ) ind 1) rest 
        | [] -> None
        
    let list_get_index_pos (x,y) arr =
        match list_get_index_single x arr with
            | None -> None
            | Some(a) -> list_get_index_single y a


    let string_split_arr string = 
        string
      |> String.split_on_char '\n'  
      |> List.filter (fun x ->(String.length (String.trim x) ) >= 1) 
      |> map (fun x -> Array.of_list (explode x)) |> Array.of_list


    let linesSplitArr f= 
        In_channel.with_open_bin f In_channel.input_all
      |> string_split_arr

    let getIndArr arr index =
      if index >= 0 && index < Array.length arr then Some( arr.(index )) else None

    let getPosArr arr (x,y)  =
        match getIndArr arr x with
        | None -> None
        | Some(a) -> getIndArr a y

    type directions =
      | North
      | South
      | East  
      | West
      | NorthEast
      | NorthWest 
      | SouthEast
      | SouthWest

    let dirToCoords dirIn =
      match dirIn with
        | North -> (0,1)
        | South -> (0,-1)
        | West -> (1,0)
        | East -> (-1,0)

        | NorthEast -> (-1,1)
        | NorthWest -> (1,1)
        | SouthEast -> (-1,-1)
        | SouthWest -> (1,-1)

    let itr_dir dir pos = 
      let p_x,p_y = pos in 
      let d_x,d_y = dirToCoords dir in
      (((+) p_x d_x),((+) p_y d_y))

    let acc_list_in_dir (dir:directions) size arr curr_pos  = 
      let getPos = getPosArr arr in
      let itr_in_dir = itr_dir dir in
      if size <=0 then None else
        let rec accList dir remainder arr curr_pos ( acc: 'a list) =
          ( if remainder = 0 then Some(acc |> List.rev) else 
            match getPos (curr_pos) with
            | None -> None 
            | Some(a) -> 
              (accList dir ((-) remainder 1) arr (itr_in_dir curr_pos) ([a] @ acc ) ))
      in accList dir size arr curr_pos [] 
    let matchLists equalityFn ( list_a: 'a list ) ( list_b: 'a list ) =
      if (List.compare_lengths list_a list_b) = 0 
        then ( List.equal (equalityFn) list_a list_b ) 
      else false
    let matchShallowLists x = matchLists (=) x
    let getBounds doubleArr = 
      let x = Array.length doubleArr in
      let y = Array.length doubleArr.(0) in
      (x,y)

    let dirList = 
      [|
        North;
        NorthEast;NorthWest;
        South;
        SouthEast;SouthWest;
        East;
        South; 
      |]



    (* End : Functions Added Since*)
end
