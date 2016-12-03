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

exception GameOver

type direction = Up | Down | Left | Right
type across = Horizontal | Vertical

let alphabet = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h';
                'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p';
                'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x';
                'y'; 'z']

let center = (7, 7)

(* A bunch of tiny utility functions. *)
let fst' (a, _, _) = a
let snd' (_, a, _) = a
let thrd' (_, _, a) = a
let to_str c = String.make 1 c


let string_of_surr s =
  let surr =
    [
      "\nLEFT: " ^ s.left^"\n";
      "RIGHT: " ^ s.right^"\n";
      "ABOVE: " ^ s.above^"\n";
      "BELOW: " ^ s.below^"\n"
    ]
  in
  String.concat "" surr


let string_of_pair (r, c) =
  "(" ^ (string_of_int r) ^ ", " ^ (string_of_int c) ^ ")\n"


let string_of_dir d =
  match d with
  | Left -> "Left\n"
  | Right -> "Right\n"
  | Up -> "Up\n"
  | Down -> "Down\n"


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


(* Reverses string [s]. *)
let reverse_str s =
  let open Str in
  let spl = Str.split (Str.regexp "") s in
  List.fold_left (fun acc b -> b ^ acc) "" spl;;


(* [valid_chars surr tiles] returns the chars from [tiles] that
 * form valid words or *ixes with the surrounding tiles in [surr].
 * NOTE: Only used for anchor generation, NOT for checking if a particular
 * tile would make a valid char given the surroundings. *)
let valid_chars surr tiles =
  let is_ok t =
    let s = to_str t in
    let ixes =
      {
        right = s ^ surr.right;
        left = s ^ surr.left |> reverse_str;
        below = s ^ surr.below;
        above = s ^ surr.above |> reverse_str;
      }
    in
    let bools =
      [
        Dictionary.in_dict ixes.right ||
        Dictionary.has_back_extensions ixes.right;
        Dictionary.in_dict ixes.left ||
        Dictionary.has_extensions ixes.left;
        Dictionary.in_dict ixes.above ||
        Dictionary.has_extensions ixes.above;
        Dictionary.in_dict ixes.below ||
        Dictionary.has_back_extensions ixes.below;
      ]
    in
    List.fold_left (fun acc b -> acc && b) true bools
  in
  List.filter is_ok tiles


(* [find_anchors b t s] returns a list of anchors given a list of
 * slots [s], a game board [b] and the player's tile list [t]. *)
let find_anchors board tiles slots =
  let aux acc slot =
    let surr = get_surroundings board slot in
    let chrs = valid_chars surr tiles in
    (slot, chrs)::acc
  in
  List.fold_left aux [] slots


(* [makes_move d s c] returns true if the char [c] makes a valid word move
 * in the direction [s] with current surroundings [s].
 * NOTE: makes_move does not concern itself with the surroundings to see
 * if a move is completely valid. valid_move is the function for that. *)
let makes_move dir surr ch =
  let s = to_str ch in
  let w =
    match dir with
    | Up | Down -> (reverse_str surr.above) ^ s ^ (surr.below)
    | Left | Right -> (reverse_str surr.left) ^ s ^ (surr.right)
  in
  Dictionary.in_dict w


(* [makes_prefix d s c] returns true if the char [c] makes a valid prefix or
 * suffix in the direction [d] with current surroundings [s].
 * NOTE: just like makes_move, makes_prefix does not check whether char [ch]
 * forms a valid move with other surroundings. valid_prefix checks for that. *)
let makes_prefix dir surr ch =
  let s = to_str ch in
  match dir with
  | Up -> (Dictionary.has_back_extensions (s ^ surr.below))
  | Down -> (Dictionary.has_extensions (s ^ surr.above |> reverse_str))
  | Left -> (Dictionary.has_back_extensions (s ^ surr.right))
  | Right -> (Dictionary.has_extensions (s ^ surr.left |> reverse_str))


(* [out_of_bounds s c] returns true if current location [curr] is out of the
 * bounds of the grid found in state [s]. *)
let out_of_bounds state curr =
  let (r, c) = curr in
  let board = state.Game.grid in
  if r > (List.length board - 1) || c > (List.length board - 1)
  then true
  else if r < 0 || c < 0 then true
  else false


let is_none o =
  match o with
  | None -> true
  | Some _ -> false


(* [invalid_pos s c] returns true if the position specified by [c] in the
 * given state [s] is not a valid position on which to place a tile. *)
let invalid_pos state curr =
  let (r, c) = curr in
  let t = Grid.get_tile (state.Game.grid) r c in
  out_of_bounds state curr || not (is_none t)


(* [get_next dir curr] returns the next location from current position [curr]
 * and in the given direction [dir]. *)
let get_next dir curr =
  let (r, c) = curr in
  match dir with
  | Up -> (r - 1, c)
  | Down -> (r + 1, c)
  | Left -> (r, c - 1)
  | Right -> (r, c + 1)


(* [search_next state dir curr] returns either the new position or None
 * in the given direction [dir] given our current position [curr]
 * and state [state]. Recursively called. *)
let rec search_next state dir curr =
  let n = get_next dir curr in
  if out_of_bounds state n then None
  else
  if invalid_pos state n then search_next state dir n
  else Some n


(* Removes only the first occurence of element [el] from list [li].
 * It is not tail recursive. *)
let rec rem li el =
  match li with
  | [] -> []
  | h::t -> if h = el then t else h::(rem t el)


(* [no_dups_append l1 l2] appends all of list [l2] contents to list [l1],
 * and it ensures that there are no duplicates from [l2], although not
 * necessarily in the result itself. *)
let no_dups_append l1 l2 =
  let rec aux l1 l2 acc =
    match l2 with
    | [] -> acc
    | h::t ->
      if List.mem h l1 || List.mem h t || List.mem h acc
      then aux l1 t acc
      else aux l1 t (h::acc)
  in
  aux l1 l2 l1


(* [other_dirs_move d s c] returns true if char [c] makes a valid move, or is
 * an otherwise acceptable tile placement in all directions given
 * surroundings [s] except for in direction [d].
 * BUG: What about a nested grid? n _ t? _ = o, but it would be rejected
 * because "ot" isn't a word. *)
let other_dirs_move dir surr c =
  let s = to_str c in
  let w =
    match dir with
    | Up | Down-> (reverse_str surr.left) ^ s ^ (surr.right)
    | Left | Right -> (reverse_str surr.above) ^ s ^ (surr.below)
  in
  if String.length w = 1 then true else Dictionary.in_dict w

let place_char state (i, j) c = Grid.place state.Game.grid i j c


(* the move MUST be valid in the direction we're building, no exceptions.
 * However, it must only be a valid move in the other directions
 * if those ones are not empty. If the surroundings are empty, we shouldn't
 * check makes_move for them. *)
let valid_move anchors dir surr curr c =
  if List.mem_assoc curr anchors
  then
    let allowed = List.assoc curr anchors in
    if List.mem c allowed
    then makes_move dir surr c && other_dirs_move dir surr c
    else false
  else makes_move dir surr c && other_dirs_move dir surr c


(* [valid_prefix d s c] returns true if char [c] makes a valid prefix
 * in direction [d] with surroundings [s]. *)
let valid_prefix dir surr c =
  makes_prefix dir surr c && other_dirs_move dir surr c


let unpack_opt o =
  match o with
  | Some a -> a
  | None -> failwith "Cannot unpack None"


(* need to write a really clear spec for this *)
let build state player anchors curr dir =
  let rec aux state player dir curr acc path =
    let (row, col) = curr in
    let tiles = player.Game.tiles in
    let surr = get_surroundings state.Game.grid (row, col) in
    let final_tiles = List.filter (valid_move anchors dir surr curr) tiles in
    let moves = List.map
        (fun t -> (place_char state (row, col) t, ((row, col), t)::path))
        final_tiles
    in
    let new_acc = no_dups_append moves acc in
    let len = List.length tiles - 1 in
    let new_curr = search_next state dir curr in
    let collector = ref new_acc in
    if new_curr = None
    then new_acc
    else
      let next = unpack_opt new_curr in
      let () =
        for i = 0 to len do
          let t = List.nth tiles i in
          let new_tiles = rem tiles t in
          let new_board = place_char state (row, col) t in
          let new_path = ((row, col), t)::path in
          let more_moves =
            if valid_prefix dir surr t
            then
              aux {state with Game.grid = new_board}
                {player with Game.tiles = new_tiles}
                dir next new_acc new_path
            else
              []
          in
          collector := no_dups_append !collector more_moves;
        done
      in
      !collector
  in
  aux state player dir curr [] []

let rank_moves moves =
  let values = Game.tile_values in
  let rank acc ((r,c), m) =
    List.assoc (Char.uppercase_ascii m) values +
    Grid.bonus_letter_at (r,c) +
    acc
  in
  let compare move1 move2 =
    let a = List.fold_left rank 0 move1 in
    let b = List.fold_left rank 0 move2 in
    b - a
  in
  List.sort compare moves


let pick_best moves =
  if List.length moves = 0 then None
  else Some (List.hd moves)


let lowercase_tiles tiles = List.map Char.lowercase_ascii tiles

let lowercase_list l =
  List.fold_left
    (
      fun acc it ->
        match it with
        | None -> None::acc
        | Some c -> (Some (Char.lowercase_ascii c))::acc
    )
    [] l |> List.rev

let lowercase_grid grid =
  let acc = ref [] in
  let len = List.length grid - 1 in
  for i = 0 to len do
    let row = List.nth grid i in
    acc := (lowercase_list row)::(!acc)
  done;
  List.rev (!acc)


let best_move state player =
  let board = state.Game.grid |> lowercase_grid in
  let tiles = player.Game.tiles |> lowercase_tiles in
  let real_state = {state with Game.grid = board} in
  let real_player = {player with Game.tiles = tiles} in
  let slots = find_slots board in
  let anchors =
    if slots = [] && board = Grid.empty then [(center, tiles)]
    else if slots = [] && board <> Grid.empty then raise GameOver
    else find_anchors board tiles slots
  in
  let build_base = build real_state real_player anchors in
  let gen_moves acc anchor =
    let ((r, c), _) = anchor in
    let stub = build_base (r, c) in
    let left_moves = stub Left in
    let right_moves = stub Right in
    let up_moves = stub Up in
    let down_moves = stub Down in
    List.rev_append acc left_moves |> List.rev_append right_moves
    |> List.rev_append up_moves |> List.rev_append down_moves
  in
  let moves =
    List.fold_left gen_moves [] anchors |> List.map (fun a -> snd a)
  in
  let ranked = rank_moves moves in
  let best = pick_best ranked in
  match best with
  | None -> raise GameOver
  | Some m ->
    {
      Game.tiles_placed = m;
      Game.player = player.Game.player_name;
      Game.swap = [];
    }

let string_of_move m =
  let tp = m.Game.tiles_placed in
  let strs =
    List.map
      (fun ((r, c), ch) -> string_of_pair (r, c) ^ to_str ch)
      tp
  in
  String.concat "\n" strs


let rec simulate_game state =
  try
    let players = state.Game.players in
    let final_state =
      List.fold_left
      (
        fun acc player ->
          let move = best_move acc player in
          let () = print_string (string_of_move move) in
          let _ = Game.execute acc move in
          acc
      )
      state players
    in
    simulate_game final_state
  with
  | GameOver -> print_string "Game has ended." |> print_newline

let run_games n =
  let game_state = Game.create_game "Kirk" "Simulation N" in
  simulate_game game_state
