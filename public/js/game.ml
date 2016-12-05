open Yojson.Basic.Util

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
  mutable turn: int;
  mutable score_history : int list
}

(* [move] is a representation of a game move containing an association list of
 * characters to be placed in specific coordinates as well as the player id of
 * the player who performs the move *)
type move = {
  tiles_placed : ((int * int) * char) list;
  player : string;
  swap : char list;
}

(* [diff] is a representation of the difference between two game states. There
 * is no field for the difference in the grids because it will always either
 * be no difference (in the case of adding/removing players or a failed move) or
 * the move given to a state *)
type diff = {
  board_diff : ((int * int) * char) list;
  new_turn_val : int;
  players_diff : player list
}

type direction = Vertical | Horizontal

(* convert a string of length 1 to character *)
let str_to_c s =
  if String.length s <> 1 then failwith "str_to_char"
  else String.get s 0

let is_over s =
  let no_tiles = List.filter (fun p -> List.length p.tiles = 0) s.players in
  (s.remaining_tiles = [] && List.length no_tiles > 0) ||
  (s.score_history = [0;0;0;0;0;0])

(* ===========================================================================
 * JSON methods below *)

(* converts a character list to its json representation. helper function for
 * state_to_json *)
let char_list_to_json lst =
  let rec aux l acc =
    match l with
    | h::[] -> acc ^ "\"" ^ (Char.escaped h) ^ "\""
    | h::t -> aux t (acc ^ "\"" ^ (Char.escaped h) ^ "\",")
    | [] -> acc
  in
  "[" ^ aux lst "" ^ "]"

let int_list_to_json lst =
  let rec aux l acc =
    match l with
    | h::[] -> acc ^ (string_of_int h)
    | h::t -> aux t (acc ^ (string_of_int h) ^ ",")
    | [] -> acc
  in
  "[" ^ aux lst "" ^ "]"

(* converts a list of [players] to its json representation. players are
 * represented with json objects.
 * helper function for state_to_json and diff_to_json *)
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
let state_to_json state =
  "{\"name\": \"" ^ state.name ^ "\",\"grid\":" ^ (Grid.to_json state.grid) ^
  ",\"players\":[" ^ (players_to_json state.players) ^ "],\"remaining_tiles\": "
  ^ (char_list_to_json state.remaining_tiles) ^ ",\"turn\": " ^
  (string_of_int state.turn) ^ ",\"score_history\": " ^
  (int_list_to_json state.score_history) ^ "}"

(* converts json list [json_l] of players to list of players.
 * helper function for state_from_json and diff_from_json *)
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
let state_from_json json =
  let n = member "name" json |> to_string in
  let g = member "grid" json |> Grid.from_json in
  let p =
    member "players" json |> to_list
    |> json_players_to_players |> List.rev
  in
  let r =
    member "remaining_tiles" json |> to_list
    |> List.map (fun x -> x |> to_string |> str_to_c)
  in
  let s =
    member "score_history" json |> to_list
    |> List.map (fun x -> x |> to_int)
  in
  let t = member "turn" json |> to_int in
  {
    name = n;
    grid = g;
    players = p;
    remaining_tiles = r;
    turn = t;
    score_history = s
  }

(* converts a [board_diff] or tiles_placed to json object. It can be used for
 * both because both are list of ((int,int),char).
 * helper function for diff_to_json and move_to_json *)
let board_diff_to_json board_diff =
  let rec aux bd acc =
    match bd with
    | ((row,col),value)::[] ->
      acc ^ "{\"row\":" ^ (string_of_int row) ^ ",\"col\":" ^
      (string_of_int col) ^ ",\"value\":\"" ^ (Char.escaped value) ^ "\"}"
    | ((row,col),value)::t ->
      (acc ^ "{\"row\":" ^ (string_of_int row) ^ ",\"col\":" ^
      (string_of_int col) ^ ",\"value\":\"" ^ (Char.escaped value) ^ "\"},")
      |> aux t
    | [] -> acc
  in
  aux board_diff ""

(* converts json representation of [tiles_placed] to list of (y,x) and chars.
 * helper function for move_from_json and diff_from_json *)
let json_tp_to_tp tiles_placed =
  let rec aux tp acc =
    match tp with
    | h::t ->
      let row = member "row" h |> to_int in
      let col = member "col" h |> to_int in
      let value = member "value" h |> to_string |> str_to_c in
      aux t (((row,col),value)::acc)
    | [] -> acc
  in
  aux tiles_placed []

(* convert diff [d] to its json representation *)
let diff_to_json d =
  "{\"board_diff\": [" ^ (board_diff_to_json d.board_diff) ^
  "],\"new_turn_val\": " ^ (string_of_int d.new_turn_val) ^
  ",\"players_diff\": [" ^ (players_to_json d.players_diff) ^ "]}"

(* [diff] converted from its json representation *)
let diff_from_json json =
  let b = member "board_diff" json |> to_list |> json_tp_to_tp |> List.rev in
  let t = member "new_turn_val" json |> to_int in
  let p = member "players_diff" json |> to_list |> json_players_to_players in
  {board_diff = b; new_turn_val = t; players_diff = p}

(* converts a move [m] to its json representation *)
let move_to_json m =
  "{\"tilesPlaced\": [" ^ (board_diff_to_json m.tiles_placed) ^
  "], \"playerName\": \"" ^ m.player ^ "\", \"swappedTiles\":" ^
  (char_list_to_json m.swap) ^ "}"

(* converts [json] to its move representation *)
let move_from_json json =
  let p = member "playerName" json |> to_string in
  let tp = member "tilesPlaced" json |> to_list |> json_tp_to_tp |> List.rev in
  let st =
    member "swappedTiles" json |> to_list
    |> List.map (fun x -> x |> to_string |> str_to_c)
  in
  {player = p; tiles_placed = tp; swap = st}
