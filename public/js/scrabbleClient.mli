
open Yojson.Basic

(* [result] contains the resulting value from a request with possible errors *)
type 'a result = Val of 'a 
                 | Exists of string
                 | Not_found of string 
                 | Full of string 
                 | Failed of string
                 | Server_error of string
                 | Success

(* [join_game player_name game_name] joins the player with name [player_name] to
 * the game with the name [game_name] and wraps the result in an Lwt thread *)
val join_game : string -> string -> Game.state result Lwt.t

(* [create_game player_name game_name] creates a game and joins the player with 
 * name [player_name] to the game with the name [game_name] and wraps the result
 * in an Lwt thread *)
val create_game : string -> string -> Game.state result Lwt.t

(* [leave_game player_name game_name] removes the player with name [player_name]
 * from the game with the name [game_name] synchronously *)
val leave_game : string -> string -> unit

(* [execute_move game_name move] executes [move] on the game with name 
 * [game_name] and updates are sent to all clients of the game accordingly *)
val execute_move : string -> Game.move -> 'a result Lwt.t

(* [subscribe_updates player_name game_name callback] subscribes a client with 
 * player name [player_name] to a [game_name] event source for game related 
 * updates with the callback [callback] to precess received JSON *)
val subscribe_updates : string -> string -> (json -> unit) -> unit

(* [send_message player_name game_name msg] sends the message [msg] to the game 
 * with name [game_name] from the player with name [player_name] *)
val send_message : string -> string -> string -> unit

(* [subscribe_messaging player_name game_name callback] subscribes a client with
 * player name [player_name] to a [game_name] event source for message updates 
 * with the callback [callback] to process received JSON *)
val subscribe_messaging : string -> string -> (json -> unit) -> unit

(* [close_sources ()] closes the event sources *)
val close_sources : unit -> unit
