
(* [save_board game] saves the [game] to the database *)
val save_game : Game.state -> unit

(* [get_game id] gets the [game] associated with [id] from the database *)
val get_game : string -> Game.state

(* [get_games ()] gets the list of games from the database *)
val get_games : unit -> Game.state list

(* [remove_game id] remove the game associated with [id] from the database *)
val remove_game : string -> unit

(* [get_players ()] gets the list of players from the database *)
val get_players: unit -> Game.player list
