
(* [board_val game] returns the integer value of the board [game] for the
 * player [player]. *)
val board_val : Game.t -> Player.t -> int

(* [best_move game player] returns the best move for the given player
 * and current board state. *)
val best_move : Game.t -> Player.t -> Game.t

(* [word_score word] returns the score for a given word independent of
 * the board being used. *)
val word_score : string -> int
