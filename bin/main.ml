let int_of_bool = function
  | 0 -> false
  | _ -> true
;;

let createBoard size =
  let rec createLine buff maxSize = 
    if List.length buff < maxSize then
      let random_int = Random.int 2 in
      createLine (buff @ [int_of_bool random_int]) maxSize
    else
      buff;
  in
  let rec createBoardAcc buff maxSize =
    if List.length buff < maxSize then
      createBoardAcc (buff @ createLine [] 0) size
    else
      buff;
  in
  createBoardAcc [] size
;;


let runGameOfLife boardSize generations =
  let board = createBoard boardSize in

