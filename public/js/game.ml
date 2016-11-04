
(* [player] contains the player's identification information, tiles, score,
 * order in the game, and a flag indicating whether this player is an AI *)
type player = {
  player_id : int;
  player_name : string;
  tiles : char list;
  score : int;
  order : int;
  ai : bool
}

(* [state] contains the game's identification information, board, players, 
 * remaining tiles (i.e. bag), and turn *)
type state = {
  id : int;
  name : string;
  grid: Grid.board;
  players : player list;
  remaining_tiles : char list;
  turn: int
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
let execute state move = 
  state (* TODO *)
