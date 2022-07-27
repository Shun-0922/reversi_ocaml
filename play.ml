open Color
open Command
open Int64

let counter = ref 4
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


(**********************************************************************************************************)

let eval_board board color ms =
  count board color


let rec negamax board color (i,j) depth =
  let new_board = doMove board (Mv (i+1,j+1)) color in
  let new_ms = valid_moves new_board (3-color) in
  if new_ms = [] || depth = 0 then -1*(eval_board new_board (3-color) new_ms)
  else -1*(negamax' new_board (opposite_color color) new_ms (depth - 1))

and negamax' board color ms depth =
  match ms with
  | [] -> -100000000
  | y :: ys -> 
    let eval_y = negamax board color y depth in
    let eval_ys = negamax' board color ys depth in
    if eval_y > eval_ys then eval_y else eval_ys


let rec select_move board color ms = 
  match ms with
  | [] -> ((-1,-1),-100000000)
  | y :: ys -> 
    let depth = if !counter < 54 then 2 else 64 - !counter in 
    let eval_y = negamax board color y depth in
    let (ys_res, m) = select_move board color ys in
    print_int(eval_y);print_string(" ");
    if eval_y > m then (y,eval_y) else (ys_res,m)
    


(**********************************************************************************************************)

let play board color =
  print_int(!counter);print_string(" <-counter\n");
  let ms = valid_moves board color in
    print_int(eval_board board color ms);print_string("\n");
    print_int(List.length(ms));print_string("\n");
    if ms = [] then
      Pass
    else
      let ((i,j),_) = select_move board color ms in
	Mv (i+1,j+1)





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
