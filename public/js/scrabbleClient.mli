
(* [Server_error] is an exception that represents errors with the server *)
exception Server_error
(* [Full] is an exception raised when full games are attempted to be joined *)
exception Full

(* [get_game_info ()] gets an association list of game id's to game names and
 * returns the list wrapped in an Lwt thread *)
val get_game_info : unit -> (int * string) list Lwt.t

(* [join_game id name] joins the player with name [name] to the game with the 
 * [id] and wraps the result in an Lwt thread *)
val join_game : int -> string -> Game.state Lwt.t

(* [create_game player_name game_name] creates a game and joins the player with 
 * name [player_name] to the game with the name [game_name] and wraps the result
 * in an Lwt thread *)
val create_game : string -> string -> Game.state Lwt.t

(* [get_game_state id] gets the state of the game with id [id] and wraps the
 * result in an Lwt thread *)
val get_game_state : int -> Game.state Lwt.t

(* [execute_move id move] executes [move] on the game with id [id] and wraps the
 * result in an Lwt thread *)
val execute_move : int -> Game.move -> Game.state Lwt.t
