(* [best_move game player] returns the best move for the given player
 * and current board state. *)
val best_move : Dictionary.t -> Dictionary.t -> Game.state -> Game.player -> Game.move
