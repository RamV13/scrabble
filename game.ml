open Yojson.Basic.Util
open Grid

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

(* [add_player state player_id player_name] adds the player with id [player_id]
 * and name [player_name] to the current game [state], and returns the new state
 * The player replaces the computer with the lowest order, and inherits its 
 * tiles, score, and turn. 
 * raise Failure if the game is full of players (non computer) already *)
let add_player s p_n = 
  let rec get_new_players players acc ai_found =
    match players with
    | h::t -> 
      if h.ai && not ai_found then 
        let new_player = {h with player_name=p_n; ai=false} in
        get_new_players t (acc @ [new_player]) true
      else 
        get_new_players t (acc @ [h]) ai_found
    | [] -> 
      if not ai_found then failwith "Game full"
      else acc
  in
  let new_players = get_new_players s.players [] false in
  {s with players=new_players}

(* [remove_player state player_id] removes the player with id [player_id] from
 * current game [state], and returns the new state. It replaces the old player
 * with a computer that inherits the removed player's tiles, score, turn, and id
 * raises Failure if there is no player in the game with [player_id] *)
let remove_player s p_id = 
  let rec get_new_players players acc pl_found =
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

(* [get_diff state state] returns the difference [diff] between two game states.
 * requires: The state id and names are equal *)
let get_diff s1 s2 = 
  failwith "unimplemented"

(* [execute state move] executes a [move] to produce a new game state from the 
 * previous game state [state] *)
let execute s m =
  failwith "unimplemented"

let char_list_to_json lst =
  let rec aux l acc =
    match l with
    | h::[] -> acc ^ "'" ^ (Char.escaped h) ^ "'"
    | h::t -> aux t (acc ^ "'" ^ (Char.escaped h) ^ "',")
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

(* [to_json state] is a json representation of [state] without the outermost
 * closing braces *)
let to_json state = 
  "{\"name\": \"" ^ state.name ^ "\",\"grid\": \"\",\"players\":[" ^ 
  (players_to_json state.players) ^ "],\"remaining_tiles\": " ^ 
  (char_list_to_json state.remaining_tiles) ^ ",\"turn\": " ^ 
  (string_of_int state.turn) ^ "}"

let str_to_c s = 
  if length s <> 1 then failwith "str_to_char"
  else get s 0

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
      aux t (new_p @ acc)
    | [] -> acc
  in
  aux json_l []

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