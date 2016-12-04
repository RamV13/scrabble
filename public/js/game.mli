
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

(* [to_json state] is a json representation of [state] as a string *)
val state_to_json : state -> string

(* [from_json Yojson.Basic.json] is a [state] converted from its json 
 * representation *)
val state_from_json : Yojson.Basic.json -> state

(* json representation of a diff *)
val diff_to_json : diff -> string

(* diff converted from its json representation *)
val diff_from_json : Yojson.Basic.json -> diff

(* json representation of a move *)
val move_to_json : move -> string

(* move converted from its json representation *)
val move_from_json : Yojson.Basic.json -> move
