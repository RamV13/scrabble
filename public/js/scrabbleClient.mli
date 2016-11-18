
(* [result] contains the resulting value from a request with possible errors *)
type 'a result = Val of 'a 
                 | Exists of string
                 | Not_found of string 
                 | Full of string 
                 | Server_error of string

(* [join_game player_name game_name] joins the player with name [player)name] to
 * the game with the name [game_name] and wraps the result in an Lwt thread *)
val join_game : string -> string -> Game.state result Lwt.t

(* [create_game player_name game_name] creates a game and joins the player with 
 * name [player_name] to the game with the name [game_name] and wraps the result
 * in an Lwt thread *)
val create_game : string -> string -> Game.state result Lwt.t

(* [get_game_state name] gets the state of the game with name [name] and wraps 
 * the result in an Lwt thread *)
val get_game_state : string -> Game.state result Lwt.t

(* [execute_move name move] executes [move] on the game with name [name] and 
 * wraps the result in an Lwt thread *)
val execute_move : string -> Game.move -> Game.state result Lwt.t

(* [send_message player_name game_name msg] sends the message [msg] to the game 
 * with name [game_name] from the player with name [player_name] *)
val send_message : string -> string -> string -> unit

(* [subscribe_messaging game_name calback] subscribes a client to a [game_name] 
 * event source with the callback [callback] to process received JSON *)
val subscribe_messaging : string -> (Yojson.Basic.json -> unit) -> unit
