
(* [save_board id game] saves the [game] associated with [id] to the database *)
val save_game : string -> Game.t -> unit

(* [get_game id] gets the [game] associated with [id] from the database *)
val get_game : string -> Game.t

(* [get_games ()] gets the list of game IDs from the database *)
val get_games : unit -> string list

(* [remove_game id] remove the game associated with [id] from the database *)
val remove_game : string -> unit

(* [save_player id game] saves the player's [id] as well as the mapping to the 
 * [game] in the database *)
val save_player: string -> Game.t -> unit

(* [get_players ()] gets the list of player names from the database *)
val get_players: unit -> string list

(* [remove_player id] removes the player with id [id] from the database *)
val remove_player: string -> unit
