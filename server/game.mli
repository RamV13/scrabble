
(* [player] contains the player's identification information, tiles, score,
 * order in the game, and a flag indicating whether this player is an AI *)
type player = {
  player_id : int;
  mutable player_name : string;
  mutable tiles : char list;
  mutable score : int;
  order : int;
  mutable ai : bool
}

(* [state] contains the game's identification information, board, players, 
 * remaining tiles (i.e. bag), and turn *)
type state = {
  id : int;
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

(* [execute state move] executes a [move] to produce a new game state from the 
 * previous game state [state] *)
val execute : state -> move -> state 

(* [to_json state] is a json representation of [state] without the outermost
 * closing braces *)
val to_json : state -> string
