open Yojson

type board = (char option) list list

type neighbors = {
  top : char option;
  bottom : char option;
  left : char option;
  right : char option;
}

(*[bonus_letter_tiles] are organized as follows:
  - ((a,b),p) such that a = row, b = column, p = point multipler*)
let bonus_letter_tiles = [((0,3),2);((0,11),2);
                          ((1,5),3);((1,9),3);
                          ((2,6),2);((2,6),2);
                          ((3,0),2);((3,7),2);((3,14),2);
                          ((5,1),3);((5,5),3);((5,9),3);((5,13),3);
                          ((6,2),2);((6,6),2);((6,8),2);((6,12),2);
                          ((7,3),2);((7,11),2);
                          ((8,2),2);((8,6),2);((8,8),2);((8,12),2);
                          ((9,1),3);((9,5),3);((9,9),3);((9,13),3);
                          ((11,0),2);((11,7),2);((11,14),2);
                          ((12,6),2);((12,8),2);
                          ((13,5),3);((13,9),3);
                          ((14,3),2);((14,11),2)
                          ]

(*[bonus_word_tiles] are organized as follows:
  - ((a,b),p) such that a = row, b = column, p = point multipler*)
let bonus_word_tiles = [((0,0),3);((0,7),3);((0,14),3);
                        ((1,1),2);((1,13),2);
                        ((2,2),2);((2,12),2);
                        ((3,3),2);((3,11),2);
                        ((4,4),2);((4,10),2);
                        ((7,0),3);((7,7),2);((7,14),3);
                        ((10,4),2);((10,10),2);
                        ((11,3),2);((11,11),2);
                        ((12,2),2);((12,12),2);
                        ((13,1),2);((13,13),2);
                        ((14,0),3);((14,7),3);((14,14),3)
                        ]

let empty = [[None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
             [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
             [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
             [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
             [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
             [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
             [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
             [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
             [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
             [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
             [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
             [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
             [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
             [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
             [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None]]


let is_empty board x y = List.nth (List.nth board x) y = None

let get_tile board x y = try (List.nth (List.nth board x) y) with _ -> None

let bonus_letter_at (y,x) = 
  try List.assoc (y,x) bonus_letter_tiles with _ -> 1

let bonus_word_at (y,x) = 
  try List.assoc (y,x) bonus_word_tiles with _ -> 1

let rec place_helper1 (row:(char option) list) y c new_row = match row with
|[] -> new_row
|h::t -> if y = 0 then place_helper1 t (0-1) c (c::new_row)
  else place_helper1 t (y-1) c (h::new_row)

let rec place_helper (board:board) x y c (result:board) = match board with
|[] -> result
|h::t-> if x = 0 then place_helper t (0-1) y c ((List.rev (place_helper1 h y c []))::result)
  else place_helper t (x-1) y c (h::result)

let place board x y c : board = List.rev (place_helper board x y (Some c) [])

let get_neighbors board x y : neighbors =
  {top = get_tile board (x-1) y;
   bottom = get_tile board (x+1) y;
   left = get_tile board x (y-1);
   right = get_tile board x (y+1)}

let rec jsonify_row row result : json = match row with
|[] -> `List (List.rev(result))
|h::t -> match h with
  |Some c -> jsonify_row t (`String (Char.escaped c) :: result)
  |None -> jsonify_row t (`String "" :: result)

let rec to_json_helper board result_board : json = match board with
|[] -> `List (List.rev(result_board))
|h::t -> to_json_helper t ((jsonify_row h []) :: result_board)

let to_json board : string =
  Yojson.to_string (to_json_helper board [])

let rec row_to_option row result = match row with
|[] -> List.rev(result)
|h::t -> if Yojson.Basic.Util.to_string h = "" then row_to_option t (None :: result)
  else row_to_option t ((Some (String.get (Yojson.Basic.Util.to_string h) 0)) :: result)

let dejsonify_row row = row_to_option (Yojson.Basic.Util.to_list row) []

let rec dejsonify board result = match board with
|[] -> List.rev(result)
|h::t -> dejsonify t ((dejsonify_row h) :: result)

let from_json j = 
  dejsonify (Yojson.Basic.Util.to_list j) []
