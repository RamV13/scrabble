open Yojson
open Grid

exception Full
exception FailedMove of string
exception PlayerExists

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

(* point values of different tiles *)
val tile_values : (char * int) list

(* initialize list of names from names.txt *)
val init_names : unit -> unit

(* create new game with 3 AIs and one user given game name and player name.
 * creates ais by getting random names from text file. Tiles are also randomly
 * distributed to all players *)
val create_game : string -> string -> state

(* [add_player state player_name] adds the player with name [player_name] to the
 * current game [state], and returns the new turn order that the player was
 * added to. The state itself is changed mutably. The player replaces any
 * computer, and inherits its tiles, score, and turn.
 * raises Full if the game is full of players already and can't be joined *)
val add_player : state -> string -> int

(* [remove_player state player_id] removes the player with id [player_id] from
 * current game [state], and returns a tuple containing the new AIs name and its
 * turn order. It replaces the old player with a computer that inherits the
 * removed player's tiles, score, turn, and id. The state itself is changed
 * mutably.
 * asserts false if there is no player in the game with [player_id] *)
val remove_player : state -> string -> (string * int)

(* [execute state move] executes a [move] in a given game [state]. The game is
 * updated mutably and the diff between the two states is returned.
 * raises FailedMove if the move fails *)
val execute : state -> move -> diff

val is_over : state -> bool

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
