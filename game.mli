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
  score_diff : int;
  added_tiles : char list;
  removed_tiles : char list;
  turn_diff : int;
  added_players : player list;
  removed_players : player list;
}

(* [add_player state player_id player_name] adds the player with id [player_id]
 * and name [player_name] to the current game [state], and returns the new state
 * The player replaces any computer, and inherits its tiles, score, and turn.
 * raise Failure if the game is full of players (non computer) already *)
val add_player : state -> string -> state

(* [remove_player state player_id] removes the player with id [player_id] from
 * current game [state], and returns the new state. It replaces the old player
 * with a computer that inherits the removed player's tiles, score, turn, and id
 * raises Failure if there is no player in the game with [player_id] *)
val remove_player : state -> string -> state

(* [get_diff state state] returns the difference [diff] between two game states.
 * requires: The state id and names are equal *)
val get_diff : state -> state -> diff

(* [execute state move] executes a [move] to produce a new game state from the
 * previous game state [state] *)
val execute : state -> move -> state

(* [to_json state] is a json representation of [state] without the outermost
 * closing braces *)
val to_json : state -> string