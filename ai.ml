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
  let len = List.length board - 1 in
  let slots = ref [] in
  for r = 0 to len do
    for c = 0 to len do
      if is_slot board r c then slots := (r,c)::!slots else ()
    done;
  done;
  !slots


let fst' (a, _, _) = a
let snd' (_, a, _) = a
let thrd' (_, _, a) = a
let to_str c = String.make 1 c
let list_place l r c ch = (ch, (r,c))::l


(* [find_adj board slot acc dir] returns the words adjacent to slot [s]
 * in the given direction [dir] for board [board]. Helper function. *)
let rec find_adj board slot acc dir =
  let (r, c) = slot in
  let n = Grid.get_neighbors board r c in
  let adj =
    match dir with
    | Left -> (n.Grid.left, (r, c - 1), Left)
    | Right -> (n.Grid.right, (r, c + 1), Right)
    | Up -> (n.Grid.top, (r - 1, c), Up)
    | Down -> (n.Grid.bottom, (r + 1, c), Down)
  in
  match fst' adj with
  | None -> acc
  | Some c -> find_adj board (snd' adj) (acc ^ (to_str c)) (thrd' adj)


(* [get_surroundings b s] returns the surrounding words around slot [s]
 * on the board [b]. *)
let get_surroundings board slot =
  let f = find_adj board slot "" in
  {
    left = f Left;
    right = f Right;
    above = f Up;
    below = f Down;
  }


(* [valid_chars surr tiles] returns the chars from [tiles] that
 * form valid words with the surrounding tiles in [surr]. *)
let valid_chars surr tiles =
  let is_ok t =
    let s = to_str t in
    let ixes =
      [
        s ^ surr.right;
        surr.left ^ s;
        s ^ surr.below;
        surr.above ^ s
      ]
    in
    let not_empties = List.filter (fun a -> String.length a > 1) ixes in
    let bools = List.map Dictionary.in_dict not_empties in
    List.fold_left (fun acc b -> acc && b) true bools
  in
  List.filter is_ok tiles


(* [get_anchors b t s] returns a list of anchors given a list of
 * slots [s], a game board [b] and the player's tile list [t]. *)
let get_anchors board tiles slots =
  let aux acc slot =
    let surr = get_surroundings board slot in
    let chrs = valid_chars surr tiles in
    (slot, chrs)::acc
  in
  List.fold_left aux [] slots


(* [makes_move d s c] returns true if the char [c] makes a valid word move
 * in the direction [s] with current surroundings [s]. *)
let makes_move dir surr ch =
  let s = to_str ch in
  match dir with
  | Up -> Dictionary.in_dict (s ^ surr.below)
  | Down -> Dictionary.in_dict (surr.above ^ s)
  | Left -> Dictionary.in_dict (s ^ surr.right)
  | Right -> Dictionary.in_dict (surr.left ^ s)


(* [makes_prefix d s c] returns true if the char [c] makes a valid prefix or
 * suffix in the direction [d] with current surroundings [s]. *)
let makes_prefix dir surr ch =
  let s = to_str ch in
  match dir with
  | Up -> (Dictionary.has_extensions (surr.below ^ s))
  | Down -> (Dictionary.has_extensions (s ^ surr.above))
  | Left -> (Dictionary.has_extensions (surr.right ^ s))
  | Right -> (Dictionary.has_extensions (s ^ surr.left))


let get_next dir curr =
  let ((r, c), cl) = curr in
  match dir with
  | Up -> ((r - 1, c), cl)
  | Down -> ((r + 1, c), cl)
  | Left -> ((r, c + 1), cl)
  | Right -> ((r, c - 1), cl)


(* Removes element [el] from list [li]. It is tail recursive. *)
let rem li el =
  let rec aux l e acc=
    match li with
    | [] -> acc
    | h::t -> if h = e then t else aux t e (h::acc)
  in
  aux li el []


(* Should return a list of board * char/int lists *)
let rec build start board curr surr tiles dir acc =
  let cl = snd curr in
  let (r, c) = fst curr in
  match cl with
  | [] -> acc
  | _::_ ->
    let new_moves =
      let good_chars = List.filter (fun c -> makes_move dir surr c) cl in
      List.fold_left
        (
          fun acc ch ->
            (Grid.place board r c ch, start, dir)::acc
        )
        [] good_chars
    in
    let new_acc = List.rev_append new_moves acc in
    let good_prefixes =
      match tiles with
      | [] -> []
      | _::_ ->
        List.filter (fun ch -> makes_prefix dir surr ch) cl
    in
    let new_curr = get_next dir curr in
    List.fold_left
      (fun a ch ->
         let new_board = Grid.place board r c ch in
         List.rev_append a
           (build start new_board
              new_curr (fst new_curr |> get_surroundings new_board)
              (rem tiles ch) dir new_acc))
      new_acc
      good_prefixes

let gen_tiles_placed board cl start dir =
  let rec aux coord counter acc =
    if counter = 0 then acc
    else
      let (r, c) = coord in
      let (nr, nc) =
        match dir with
        | Up -> (r - 1, c)
        | Down -> (r + 1, c)
        | Left -> (r, c - 1)
        | Right -> (r, c + 1)
      in
      match Grid.get_tile board nr nc with
      | Some ch -> aux (nr, nc) (counter - 1) ((ch, (nr, nc))::acc)
      | None -> failwith "Board error"
  in
  aux start (List.length cl) []

let flip' (a, b) = (b, a)

(* Get the diff of all boards, generate moves as a result *)
let to_moves player init_state boards =
  List.fold_left
    (
      fun acc b ->
        let (board, start, dir) = b in
        let added =
          Grid.get_diff init_state.Game.grid board
        in
        let open Game in
        let mv =
          {
            Game.tiles_placed = gen_tiles_placed board added start dir |> List.map flip';
            player = player.player_name;
          }
        in
        mv::acc
    )
    [] boards

let rank_moves moves = List.sort (fun a b -> 0) moves

let center = (7, 7)

let best_move state player =
  let init_board = state.Game.grid in
  let init_tiles = player.Game.tiles in
  let slots = find_slots init_board in
  if slots = [] then
    let mv = build center init_board (center, player.Game.tiles)
        (get_surroundings init_board center)
        init_tiles Right []
    in
    mv |> to_moves player state  |> rank_moves |> List.hd
  else
    let anchors = get_anchors init_board init_tiles slots in
    let moves = List.fold_left
        (
          fun acc anc ->
            let b = build (fst anc) init_board anc
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
    moves |> to_moves player state  |> rank_moves |> List.hd
