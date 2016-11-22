open Yojson
open Grid

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
  tiles_placed : ((int * int) * char) list;
  player : string
}

(* [diff] is a representation of the difference between two game states. There
 * is no field for the difference in the grids because it will always either
 * be no difference (in the case of adding/removing players or a failed move) or
 * the move given to a state *)
type diff = {
  board_diff = ((int * int) * char) list
  new_turn_val : int;
  players_diff : player list
}

(* create new game with 4 AIs given game name and player name. creates ais
 * by getting random names from text file *)
val create_game : string -> string -> state

(* [add_player state player_id player_name] adds the player with id [player_id]
 * and name [player_name] to the current game [state], and returns the new state
 * The player replaces any computer, and inherits its tiles, score, and turn.
 * raise Failure if the game is full of players (non computer) already 
 * now its mutable and just return that new players order raises Full exception *)
val add_player : state -> string -> int

(* [remove_player state player_id] removes the player with id [player_id] from
 * current game [state], and returns the new state. It replaces the old player
 * with a computer that inherits the removed player's tiles, score, turn, and id
 * raises Failure if there is no player in the game with [player_id] 
 * now its mutable and return the new ai player name and its order *)
val remove_player : state -> string -> (string * int)

(* [execute state move] executes a [move] to produce a new game state from the
 * previous game state [state] 
* now its mutable raise FailedMove if move failed *)
val execute : state -> move -> diff

(* [to_json state] is a json representation of [state] as a string *)
val to_json : state -> string

(* [from_json Yojson.Basic.json] is a [state] converted from its json 
 * representation *)
val from_json : Yojson.Basic.json -> state

(* json representation of a diff *)
val diff_to_json : diff -> string

(* move converted from its json representation *)
val move_from_json : Yojson.Basic.json -> move

val init_names : unit -> unit