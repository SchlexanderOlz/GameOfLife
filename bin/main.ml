let createBoard size =
  let rec createLine buff maxSize = 
    if List.length buff < maxSize then
      let random_float = Random.float 1.0 in
      let cell_value = if random_float < 0.3 then true else false in
      createLine (cell_value :: buff) maxSize
    else
      List.rev buff;
  in
  let rec createBoardAcc buff maxSize =
    if List.length buff < maxSize then
      createBoardAcc (createLine [] maxSize :: buff) size
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
  | x :: y -> displayCell x; print_char '\n'; displayRow y
in
  displayRow board;
  print_newline ()


let updateBoard (board : bool list list) : bool list list = 
  let neighbors = [(-1, -1); (-1, 0); (-1, 1);
                    (0, -1);          (0, 1);
                    (1, -1); (1, 0); (1, 1)] in
  let rowLength = List.length (List.nth board 0) in
  let columnLength = List.length board in 
  let rec updateCells buff row column = 
    if columnLength = column then List.rev buff else
    let getElement row column =
      if column >= rowLength || row >= columnLength || row < 0 || column < 0 then false else
      List.nth (List.nth board row) column
    in
    let neighborCount = List.fold_left (fun neighbour (y, x) -> if getElement (row + y) (column + x) then neighbour + 1 else neighbour) 0 neighbors in
    match (getElement row column, neighborCount) with
    | (false, 3) -> updateCells (true :: buff) row (column + 1)
    | (true, x) when x = 3 || x = 2 -> updateCells (true :: buff) row (column + 1)
    | _ -> updateCells (false :: buff) row (column + 1)   
  in
  let rec updateLines buff row = 
    if columnLength = row then buff else
      updateLines ((updateCells [] row 0) :: buff) (row + 1)
  in
  List.rev (updateLines [] 0)
;;


let runGameOfLife boardSize generations =
  let board = createBoard boardSize in
  let rec runGenerations board = function
    | 0 -> ()
    | x -> displayBoard board; Unix.sleepf 0.1; runGenerations (updateBoard board) (x - 1)
  in
  runGenerations board generations
;;

runGameOfLife 100 10000
