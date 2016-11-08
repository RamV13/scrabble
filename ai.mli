
(* [best_move game player] returns the best move for the given player
 * and current board state. *)
val best_move : Game.state -> Game.player -> Game.move

(* [word_score word] returns the score for a given word independent of
 * the board being used. *)
val word_score : string -> int
