open Yojson.Basic.Util

exception Full
exception FailedMove

(* [player] contains the player's identification information, tiles, score,
 * order in the game, and a flag indicating whether this player is an AI *)
type player = {
  mutable player_name : string;
  mutable tiles : char list;
  mutable score : int;
  order : int;
  mutable ai : bool
}

(* [state] contains the game's identification information, board, players, 
 * remaining tiles (i.e. bag), and turn *)
type state = {
  name : string;
  mutable grid: Grid.board;
  players : player list;
  mutable remaining_tiles : char list;
  mutable turn: int
}

(* [move] is a representation of a game move containing an association list of
 * characters to be placed in specific coordinates as well as the player id of
 * the player who performs the move *)
type move = {
  tiles_placed : (char * (int * int)) list;
  player : int
}

(* [diff] is a representation of the difference between two game states. There
 * is no field for the difference in the grids because it will always either
 * be no difference (in the case of adding/removing players or a failed move) or
 * the move given to a state *)
type diff = {
  score_diff : int list;
  added_tiles : char list;
  removed_tiles : char list;
  turn_diff : int;
  players_diff : player list
}

let create_bag () = 
  (* todo this is off *)
  [
    'a';'a';'a';'a';'a';'a';'a';'a';'a';
    'b';'b';
    'c';'c';
    'd';'d';'d';'d';
    'e';'e';'e';'e';'e';'e';'e';'e';'e';'e';'e';'e';
    'f';'f';
    'g';'g';'g';
    'h';'h';
    'i';'i';'i';'i';'i';'i';'i';'i';'i';
    'j';
    'k';'k';
    'l';'l';'l';'l';
    'm';'m';
    'n';'n';'n';'n';'n';'n';
    'o';'o';'o';'o';'o';'o';'o';'o';
    'p';'p';
    'q';
    'r';'r';'r';'r';'r';'r';
    's';'s';'s';'s';
    't';'t';'t';'t';'t';'t';
    'u';'u';'u';'u';
    'v';'v';
    'w';'w';
    'x';
    'y';'y';
    'z';
    '?';'?'
  ]

(* [add_player state player_id player_name] adds the player with name 
 * [player_name] to the current game [state], and returns the turn order that
 * the player was added to (i.e. the turn of the AI who the player replaced).
 * The player replaces the computer with the lowest order, and inherits its 
 * tiles, score, and turn. 
 * raise Failure if the game is full of players (non computer) already *)
let add_player s p_n = 
  let substituted = 
      try List.find (fun player -> player.ai) s.players
      with Not_found -> raise Full
  in
  substituted.player_name <- p_n;
  substituted.ai <- false;
  substituted.order

(* [remove_player state player_id] removes the player with id [player_id] from
 * current game [state], and returns the new state. It replaces the old player
 * with a computer that inherits the removed player's tiles, score, turn, and id
 * raises Failure if there is no player in the game with [player_id] *)
let remove_player s p_n = 
  (* let rec get_new_players players acc pl_found =
    match players with
    | h::t ->
      if h.player_name = p_id then 
        let new_ai = 
          {h with player_name="Computer "^(string_of_int h.order); ai=true} in
        get_new_players t (acc @ [new_ai]) true
      else
        get_new_players t (acc @ [h]) pl_found
    | [] -> 
      if not pl_found then failwith "Player not found"
      else acc
  in
  let new_players = get_new_players s.players [] false in
  {s with players=new_players}
  *)
  failwith "unimplemented"

(* [execute state move] executes a [move] to produce a new game state from the 
 * previous game state [state] *)
let execute s m =
  failwith "unimplemented"

let char_list_to_json lst =
  let rec aux l acc =
    match l with
    | h::[] -> acc ^ "\"" ^ (Char.escaped h) ^ "\""
    | h::t -> aux t (acc ^ "\"" ^ (Char.escaped h) ^ "\",")
    | [] -> acc
  in
  "[" ^ aux lst "" ^ "]"

let players_to_json players =
  let rec aux p acc =
    match p with
    | h::[] -> 
      acc ^ "{\"player_name\":\"" ^ h.player_name ^ "\",\"tiles\":" ^ 
      (char_list_to_json h.tiles) ^ ",\"score\": " ^ (string_of_int h.score) ^ 
      ",\"order\": " ^ (string_of_int h.order) ^ ",\"ai\": " ^ 
      (string_of_bool h.ai) ^"}"
    | h::t -> 
      (acc ^ "{\"player_name\":\"" ^ h.player_name ^ "\",\"tiles\":" ^ 
      (char_list_to_json h.tiles) ^ ",\"score\": " ^ (string_of_int h.score) ^ 
      ",\"order\": " ^ (string_of_int h.order) ^ ",\"ai\": " ^ 
      (string_of_bool h.ai) ^"},") 
      |> aux t
    | [] -> acc
  in
  aux players ""

(* [to_json state] is a json representation of [state] *)
let to_json state = 
  "{\"name\": \"" ^ state.name ^ "\",\"grid\":" ^ (Grid.to_json state.grid) ^ 
  ",\"players\":[" ^ (players_to_json state.players) ^ "],\"remaining_tiles\": " 
  ^ (char_list_to_json state.remaining_tiles) ^ ",\"turn\": " ^ 
  (string_of_int state.turn) ^ "}"

let str_to_c s = 
  if String.length s <> 1 then failwith "str_to_char"
  else String.get s 0

let json_players_to_players json_l =
  let rec aux j_l acc = 
    match j_l with
    | h::t -> 
      let new_p = 
        {
          player_name = member "player_name" h |> to_string;
          tiles = List.map 
            (fun x -> x |> to_string |> str_to_c) (member "tiles" h |> to_list);
          score = member "score" h |> to_int;
          order = member "order" h |> to_int;
          ai = member "ai" h |> to_bool;
        } 
      in
      aux t (new_p :: acc)
    | [] -> acc
  in
  aux json_l []

(* [from_json Yojson.Basic.json] is a [state] converted from its json 
 * representation *)
let from_json json = 
  let n = member "name" json |> to_string in
  let g = member "grid" json |> Grid.from_json in
  let p = member "players" json |> to_list |> json_players_to_players in
  let r = 
    member "remaining_tiles" json 
    |> to_list 
    |> List.map (fun x -> x |> to_string |> str_to_c) 
  in
  let t = member "turn" json |> to_int in
  {
    name = n;
    grid = g;
    players = p;
    remaining_tiles = r;
    turn = t
  }
(* for testing *)
(* 
let _ = 
  to_json { name = "game"; grid = Grid.empty; players = []; remaining_tiles = ['a'; 'b']; turn = 0}
  |> Yojson.Basic.from_string |> from_json |> to_json |> print_endline
*)

(* COPIED STUFF *)

(* for initializing names to be used in create game *)
(* [names_file] is the file containing a list of line separated names *)
let names_file = "names.txt"
(* [names] is the list of computer names *)
let names = ref []

(* given bag and n # of tiles to take, return tiles taken and new bag *)
let take_tiles bag num_to_take = 
  let take_tile bag index = 
    let count = ref 0 in
    let (tile, new_bag) = List.fold_left 
      (fun (t,b) c -> 
        if !count = index && t = None then (Some c,b)
        else (count := !count + 1; (t, c::b)))
      (None,[]) bag
    in
    match tile with 
    | None -> assert false
    | Some t -> (t,List.rev new_bag)
  in
  Random.self_init ();
  let rec aux tiles b n =
    if n = 0 then (tiles,b)
    else 
      let (t,bag') = take_tile b (Random.int (List.length b)) in 
      aux (t::tiles) bag' (n - 1)
  in
  aux [] bag num_to_take

let init_names () = 
  let input_channel = open_in names_file in
  try
    let rec process_line () = 
      let line = input_line input_channel in
      names := line::!names;
      process_line ()
    in
    ignore (process_line ());
    close_in input_channel
  with
  | End_of_file -> close_in input_channel
  | exc -> close_in_noerr input_channel; raise exc

let create_game p_n g_n = 
  let grid = Grid.empty in
  let bag = create_bag () in
  let base_player = {player_name=""; tiles=[]; score=0; order=0; ai=true} in
  let create_ai order bag =
    let player_name = (List.hd !names) ^ " (AI)" in
    names := List.tl !names;
    let (tiles,bag') = take_tiles bag 7 in 
    ({base_player with player_name; order; tiles}, bag')
  in
  let (players,new_bag) = 
    let rec add_players acc order b = 
      if order < 4 then 
        let (new_player,b') = create_ai order b in
        add_players (new_player::acc) (order + 1) b'
      else (acc,b)
    in
    let (human_tiles,bag') = take_tiles bag 7 in
    add_players 
      [{base_player with player_name = p_n; ai=false; tiles = human_tiles}] 
      1 bag'
  in
  {name=g_n; grid; players=List.rev players; remaining_tiles=new_bag; turn=0}

let diff_to_json d = 
  "hi"

let move_from_json j = 
  failwith "unimplemented"

let _ = 
  init_names ();
  create_game "Brian" "mygame" |> to_json |> print_endline


