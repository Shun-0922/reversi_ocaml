open Color
open Command
open Int64


type board = int64 * int64 (*黒、白*)

(**********************************************************************************************************)

let is_piece_on_board board color (i,j) = (*i,jにcolorの駒が置いてあるか返す*)
  match board with 
  | (black_board, white_board) ->
    let color_board = (if color = black then black_board else white_board) in
    if logand (shift_left 0x0000000000000001L (i + 8*j)) color_board = 0x0000000000000000L then false else true

let color_piece_on_board board (i,j) =
  if is_piece_on_board board black (i,j) then black
  else if is_piece_on_board board white (i,j) then white
  else none

let flip_board board color ms = (*msの座標をcolorにする*)
  match board with 
  | (black_board, white_board) ->
    let f1 b64 (i,j) = logor b64 (shift_left 0x0000000000000001L (i + 8*j)) in 
    let f2 b64 (i,j) = logxor (f1 b64 (i,j)) (shift_left 0x0000000000000001L (i + 8*j)) in 
    if color = black then
      (List.fold_left f1 black_board ms, List.fold_left f2 white_board ms)
    else
      (List.fold_left f2 black_board ms, List.fold_left f1 white_board ms)


(**********************************************************************************************************)

let init_board () = (*初期化*)
  (0x0000000810000000L,0x0000001008000000L)

let dirs = [ (-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1) ]

let flippable_indices_line board color (di,dj) (i,j) = (*i-di,j-djにおいたときdi,djの方向でひっくり返せるマスのリストを返す*)
  let ocolor = opposite_color color in
  let rec f (di,dj) (i,j) r =
    if (i < 0 || j < 0 || i > 7 || j > 7) then
      []
    else if is_piece_on_board board ocolor (i,j) then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else
      []
  and    g (di,dj) (i,j) r =
    if (i < 0 || j < 0 || i > 7 || j > 7) then
      []
    else if is_piece_on_board board ocolor (i,j) then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else if is_piece_on_board board color (i,j) then
      r
    else
      [] in
    f (di,dj) (i,j) []



let flippable_indices board color (i,j) = (*i,jに置いたときにひっくり返されるマスのリストと返す*)
  let bs = List.map (fun (di,dj) -> flippable_indices_line board color (di,dj) (i+di,j+dj)) dirs in
    List.concat bs

let is_effective board color (i,j) = (*i,jに置いたときに何かをひっくり返せるかできるかどうかを返す*)
  match flippable_indices board color (i,j) with
      [] -> false
    | _  -> true

let is_valid_move board color (i,j) = (*i,jにおけるかどうかを返す*)
  i >= 0 && i < 8 && j >= 0 && j < 8 && is_piece_on_board board black (i,j) = false && is_piece_on_board board white (i,j) = false && is_effective board color (i,j)


let doMove board com color = (*boardとcommandとcolorを受け取って実行した新しいboardを返す*)
  match com with
      GiveUp  -> board
    | Pass    -> board
    | Mv (i,j) ->
	let ms = flippable_indices board color (i-1,j-1) in
	flip_board board color ((i-1,j-1)::ms)

let mix xs ys = (*直積*)
  List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)


let valid_moves board color = (*おける座標を返す*)
  let ls = [0;1;2;3;4;5;6;7] in
  List.filter (is_valid_move board color)
    (mix ls ls)

(**********************************************************************************************************)

let select_move board color ms = 0
  (*Random.int (List.length ms)*)

(**********************************************************************************************************)

let play board color =
  let ms = valid_moves board color in
    if ms = [] then
      Pass
    else
      let (i,j) = List.nth ms (select_move board color ms) in
	Mv (i+1,j+1)

let count board color = (*board上のcolorのマスの数を返す*)
  let s = ref 0 in
    for i=0 to 7 do
      for j=0 to 7 do
        if is_piece_on_board board color (i,j) then s := !s + 1
      done
    done;
    !s


let print_board board =
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for j=0 to 7 do
    print_int (j+1); print_string "|";
    for i=0 to 7 do
      print_color (color_piece_on_board board (i,j)); print_string " "
    done;
    print_endline ""
  done;
  print_endline "  (X: Black,  O: White)"


let report_result board =
  let _ = print_endline "========== Final Result ==========" in
  let bc = count board black in
  let wc = count board white in
    if bc > wc then
      print_endline "*Black wins!*"
    else if bc < wc then
      print_endline "*White wins!*"
    else
      print_endline "*Even*";
    print_string "Black: "; print_endline (string_of_int bc);
    print_string "White: "; print_endline (string_of_int wc);
    print_board board
