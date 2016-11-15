
open Game
open Sqlite3

let db_name = "scrabble.db"
let exists = Sys.file_exists db_name
let db = db_open db_name

(* creates the table in the database if it is not already present *)
let _ = 
  if not exists then
    ignore (
        exec db "CREATE TABLE Games (
                  ID INT PRIMARY KEY NOT NULL,
                  NAME VARCHAR(255) NOT NULL,
                  GRID TEXT NOT NULL,
                  PLAYERS VARCHAR(255) NOT NULL,
                  BAG TEXT NOT NULL,
                  TURN INT
                )"
    )

(* [sql_of_game game] is the SQL string value of a game *)
let sql_of_game game = 
  ""

(* [game_of_sql sql] is the game value of the SQL string value *)
let game_of_sql sql = 
  {name="";grid=Grid.empty;players=[];remaining_tiles=[];turn=0}

let save_game game = 
  ()

let get_game id = 
  {name="";grid=Grid.empty;players=[];remaining_tiles=[];turn=0}

let get_games () = 
  []

let remove_game id = 
  ()
