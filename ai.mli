(* Notes for the AI:
 * An anchor is defined as an empty tile that is adjacent to a tile that
 * already has a letter in it.
 * The AI works as follows:
 * 1. Generate a list of anchor tiles, valid or not (called slots)
 * 2. Determine which slots we can actually use given our tile set
 * 3. Generate a list of anchors and the letters we can put on each
 * 4. For each anchor generate a list of possible words we can place
 * 5. Rank these possible placements and choose the best one *)

(* point type synonym for clarity later on *)
type point = (int * int)

(* placement represents a possible word placement on the board *)
type placement = (point * char) list

(* anchor represents an anchor tile's coordinates on the game board
 * and the valid chars that can be placed there. *)
type anchor = (point * (char list))

(* [find_anchors s] returns a list of locations on the board where anchor
 * tiles can be placed along with the letters that can be put there. *)
val find_anchors : Game.state -> point list -> anchor list

(* [valid_ending dict str] returns true if a string forms a valid, possible
 * word suffix. *)
val valid_ending : Dictionary.t -> string -> bool

(* [can place s a p c] returns true if char [c] can be placed at point [p]
 * for game state [s] and anchor list [a]. Makes use of valid_ending. *)
val can_place : Game.state -> anchor list -> point -> char -> bool

(* [generate_words t a al] returns a list of possible word placements
 * on the board. Uses the anchor list to optimize. *)
val generate_words : Game.player -> anchor -> placement list

(* Given a board and bonus tiles, give the word placement a score *)
val score_placement : Grid.board -> Grid.bonus_tiles -> placement -> int

(* [best_move game player] returns the best move for the given player
 * and current board state. *)
val best_move : Game.state -> Game.player -> Game.move
