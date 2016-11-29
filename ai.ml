(* Some definitions for the AI:
 * A slot is an empty tile that is adjacent to at least another tile that
 * has a letter in it. Anchors are slots with a tagged list of chars. *)

(* Type synonym for clarity later on *)
type point = int * int

(* An anchor represents a point on the board adjacent to an existing tile
 * along with the characters that can be placed on it. *)
type anchor = point * char list

type surroundings = {
  left : string;
  right : string;
  above : string;
  below : string
}

type direction = Up | Down | Left | Right


(* [has_neighbors n] returns true if at least one of [neighbors] n
 * is not empty. *)
let has_neighbors n =
  if (n.Grid.top = None) && (n.Grid.left = None) &&
     (n.Grid.right = None) && (n.Grid.bottom = None)
  then false
  else true

(* [is_slot b r c] returns true if the tile at (r,c) on board [b] is a slot. *)
let is_slot board r c =
  if Grid.is_empty board r c
  then
    let n = Grid.get_neighbors board r c in
    if has_neighbors n then true
    else false
  else false

(* [find_slots b] returns a list of slots (represented as point list) for
 * the grid board [b]. *)
let find_slots board =
  let rec process_row row_num col_num row acc =
    match row with
    | [] -> acc
    | h::t ->
      match h with
      | None -> process_row row_num (col_num + 1) t acc
      | Some c ->
        if is_slot board row_num col_num
        then process_row row_num (col_num + 1) t ((row_num, col_num)::acc)
        else process_row row_num (col_num + 1) t acc
  in
  let rec aux row_num acc board =
    match board with
    | [] -> acc
    | h::t -> aux (row_num + 1) ((process_row row_num 1 h []) @ acc) t
  in
  aux 1 [] board

let fst' (a, _, _) = a
let snd' (_, a, _) = a
let thrd' (_, _, a) = a

let rec find_adj board slot acc d =
  let open Grid in
  let (r, c) = slot in
  let n = Grid.get_neighbors board r c in
  let adj =
    match d with
    | Left -> (n.left, (r, c - 1), Left)
    | Right -> (n.right, (r, c + 1), Right)
    | Up -> (n.top, (r - 1, c), Up)
    | Down -> (n.bottom, (r + 1, c), Down)
  in
  match fst' adj with
  | None -> acc
  | Some c -> find_adj board (snd' adj) (acc ^ (String.make 1 c)) (thrd' adj)

let get_surroundings board slot =
  let f = find_adj board slot "" in
  {
    left = f Left;
    right = f Right;
    above = f Up;
    below = f Down;
  }

let to_str c = String.make 1 c

let valid_chars pd sd surr tiles =
  let pred t =
    let st = to_str t in
    Dictionary.mem (surr.left ^ st) pd &&
    Dictionary.mem (st ^ surr.right) sd &&
    Dictionary.mem (surr.above ^ st) pd &&
    Dictionary.mem (st ^ surr.below) sd &&
    Dictionary.mem (surr.left ^ st ^ surr.right) pd &&
    Dictionary.mem (surr.above ^ st ^ surr.below) pd
  in
  List.filter pred tiles

let get_anchors board tiles pd sd slots =
  let aux acc slot =
    let surr = get_surroundings board slot in
    let chrs = valid_chars pd sd surr tiles in
    (slot, chrs)::acc
  in
  List.fold_left aux [] slots

let makes_move pd sd dir surr ch =
  let s = to_str ch in
  match dir with
  | Up -> Dictionary.mem (s ^ surr.below) sd
  | Down -> Dictionary.mem (surr.above ^ s) pd
  | Left -> Dictionary.mem (s ^ surr.right) sd
  | Right -> Dictionary.mem (surr.left ^ s) pd


let makes_prefix pd sd dir surr ch =
  let s = to_str ch in
  match dir with
  | Up -> (Dictionary.extensions (surr.below ^ s) pd) <> []
  | Down -> (Dictionary.extensions (s ^ surr.above) sd) <> []
  | Left -> (Dictionary.extensions (surr.right ^ s) pd) <> []
  | Right -> (Dictionary.extensions (s ^ surr.left) sd) <> []

let get_next pd sd dir curr =
  let ((r, c), cl) = curr in
  match dir with
  | Up -> ((r - 1, c), cl)
  | Down -> ((r + 1, c), cl)
  | Left -> ((r, c + 1), cl)
  | Right -> ((r, c - 1), cl)

let rem li el =
  let rec aux l e acc=
    match li with
    | [] -> acc
    | h::t -> if h = e then t else aux t e (h::acc)
  in
  aux li el []

let list_place l r c ch = (ch, (r,c))::l

let rec build board pd sd curr surr tiles dir acc =
  let cl = snd curr in
  let (r, c) = fst curr in
  match cl with
  | [] -> acc
  | _::_ ->
    let new_moves =
      let good_chars = List.filter (fun c -> makes_move pd sd dir surr c) cl in
      List.fold_left (fun acc ch -> (Grid.place board r c ch)::acc) [] good_chars
    in
    let new_acc = List.rev_append new_moves acc in
    let good_prefixes =
      match tiles with
      | [] -> []
      | _::_ ->
        List.filter (fun ch -> makes_prefix pd sd dir surr ch) cl
    in
    let new_curr = get_next pd sd dir curr in
    List.fold_left
      (fun a ch ->
         let new_board = Grid.place board r c ch in
         List.rev_append a
           (build new_board
              pd sd new_curr (fst new_curr |> get_surroundings new_board)
              (rem tiles ch) dir new_acc))
      new_acc
      good_prefixes

let rank_boards boards = List.sort (fun a b -> 0) boards

let best_move pd sd state player =
  let init_board = state.Game.grid in
  let init_tiles = player.Game.tiles in
  let slots = find_slots init_board in
  let anchors = get_anchors init_board init_tiles pd sd slots in
  let moves = List.fold_left
      (
        fun acc anc ->
          let b = build init_board pd sd anc
              (get_surroundings init_board (fst anc))
              init_tiles
          in
          let lm = b Left [] in
          let rm = b Right [] in
          let um = b Up [] in
          let dm = b Down [] in
          let mvs =
            lm |> List.rev_append rm |> List.rev_append um |> List.rev_append dm
          in
          List.rev_append mvs acc
      )
      [] anchors
  in
  List.hd (rank_boards moves)
