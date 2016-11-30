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

(* A bunch of tiny utility functions. *)
let fst' (a, _, _) = a
let snd' (_, a, _) = a
let thrd' (_, _, a) = a
let to_str c = String.make 1 c
let list_place l r c ch = (ch, (r,c))::l
let flip' (a, b) = (b, a)

let alphabet = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h';
                'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p';
                'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x';
                'y'; 'z']

(* [has_neighbors n] returns true if at least one of neighbors [n]s
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
        s ^ surr.left;
        s ^ surr.below;
        s ^ surr.above
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
  | Up -> (Dictionary.has_back_extensions (surr.below ^ s))
  | Down -> (Dictionary.has_extensions (surr.above ^ s))
  | Left -> (Dictionary.has_back_extensions (surr.right ^ s))
  | Right -> (Dictionary.has_extensions (surr.left ^ s))


(* [out_of_bounds s c] returns true if current location [curr] is out of the
 * bounds of the grid found in state [s]. *)
let out_of_bounds state curr =
  let ((r, c), _) = curr in
  let board = state.Game.grid in
  if r > (List.length board - 1) || c > (List.length board - 1)
  then true
  else if r < 0 || c < 0 then true
  else false


(* [get_next dir curr] returns the next location from current position [curr]
 * and in the given direction [dir]. *)
let get_next dir curr =
  let ((r, c), _) = curr in
  match dir with
  | Up -> ((r - 1, c), snd curr)
  | Down -> ((r + 1, c), snd curr)
  | Left -> ((r, c - 1), snd curr)
  | Right -> ((r, c + 1), snd curr)


(* Removes only the first occurence of element [el] from list [li].
 * It is not tail recursive. *)
let rec rem li el =
  match li with
  | [] -> []
  | h::t -> if h = el then t else h::(rem t el)


(* [intersect l1 l2] returns the elements that are common to both of
 * lists l1 AND l2. *)
let intersect l1 l2 =
  let it = l1 in
  let rec aux it acc =
    match it with
    | [] -> acc
    | h::t -> if List.mem h l2 then aux t (h::acc) else aux t acc
  in
  aux it []

(* A printing function for the type surroundings [s]. *)
let print_surr s =
  let _ =
    List.map print_string
      [s.left^"\n"; s.right^"\n"; s.above^"\n"; s.below^"\n"] in ()


(* print_pair (r,c) prints the integer pair. *)
let print_pair (r, c) =
  print_int r;
  print_newline ();
  print_int c;
  print_newline ()

(* Prints a boolean. *)
let print_bool b =
  match b with
  | true -> print_string "True"
  | false -> print_string "False"


(* [no_dups_append l1 l2] appends all of list [l2] contents to list [l1],
 * but it ensures that there are no duplicates in the result. *)
let no_dups_append l1 l2 =
  let rec aux l1 l2 acc =
    match l2 with
    | [] -> acc
    | h::t -> if List.mem h l1 then aux l1 t acc else aux l1 t (h::acc)
  in
  aux l1 l2 l1


(* need to write a really clear spec for this *)
let build state player anchors curr dir =
  let rec aux state player dir curr acc =
    let ((row, col), _) = curr in
    let valid_move surr curr c =
      try
        let allowed = List.assoc (fst curr) anchors in
        if List.mem c allowed then
          makes_move dir surr c
        else false
      with
      | Not_found -> makes_move dir surr c
    in
    let place_char state (i, j) c =
      Grid.place (state.Game.grid) i j c
    in
    let tiles = player.Game.tiles in
    let surr = get_surroundings state.Game.grid (row, col) in
    let final_tiles = List.filter (valid_move surr curr) tiles in
    let moves = List.map (place_char state (row, col)) final_tiles in
    let new_acc = no_dups_append moves acc in
    let len = List.length tiles - 1 in
    let new_curr = get_next dir curr in
    let collector = ref [] in
    collector := new_acc;
    if out_of_bounds state new_curr then new_acc else
      let () =
      for i = 0 to len do
        let t = List.nth tiles i in
        let new_tiles = rem tiles t in
        let _ = List.map print_char new_tiles in
        let () = print_newline () in
        let new_board = place_char state (row, col) t in
        let more_moves =
          if makes_prefix dir surr t
          then
            aux {state with Game.grid = new_board}
            {player with Game.tiles = new_tiles} dir new_curr new_acc
          else
            []
        in
        collector := no_dups_append !collector more_moves;
      done
      in
      !collector
  in
  aux state player dir curr []
