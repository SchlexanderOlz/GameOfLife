let int_of_bool = function
  | 0 -> false
  | _ -> true
;;

let createBoard size =
  let rec createLine buff maxSize = 
    if List.length buff < maxSize then
      let random_int = Random.int 2 in
      createLine (int_of_bool random_int :: buff) maxSize
    else
      List.rev buff;
  in
  let rec createBoardAcc buff maxSize =
    if List.length buff < maxSize then
      createBoardAcc (createLine [] 0 :: buff) size
    else
      List.rev buff;
  in
  createBoardAcc [] size
;;


let displayBoard board =
  let _ = Sys.command "clear" in

  let rec displayCell = function
  | [] -> ()
  | x :: y -> if x then print_char '#' else print_char '.'; displayCell y
in
  let rec displayRow = function
  | [] -> ()
  | x :: y -> displayCell x; displayRow y
in
  displayRow board


let updateBoard board = 
  let size = Array.length board in
  let new_board = Array.copy board in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      let neighbors = [(i-1, j-1); (i-1, j); (i-1, j+1);
                      (i, j-1);               (i, j+1);
                      (i+1, j-1); (i+1, j); (i+1, j+1)] in

      let live_neighbors = 
        List.fold_left (fun count (x, y) ->
          if 0 <= x && x < size && 0 <= y && y < size && board.(x).(y) then
            count + 1
          else
            count
        ) 0 neighbors in

      if board.(i).(j) then
        if live_neighbors < 2 || live_neighbors > 3 then
          new_board.(i).(j) <- false
      else if live_neighbors = 3 then
        new_board.(i).(j) <- true
    done;
  done;
  new_board
;;


let runGameOfLife boardSize generations =
  let listBoard = createBoard boardSize in
  let board = Array.of_list (List.map (fun x -> Array.of_list x) listBoard) in
  let rec runGenerations board = function
    | 0 -> ()
    | x -> displayBoard board; runGenerations (updateBoard board) x 
  in
  runGenerations board generations
;;

