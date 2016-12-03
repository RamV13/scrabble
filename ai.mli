type point = int * int
type anchor = point * char list
type surroundings = {
  left : string;
  right : string;
  above : string;
  below : string;
}
exception GameOver
type direction = Up | Down | Left | Right
type across = Horizontal | Vertical
val alphabet : char list
val center : int * int
val fst' : 'a * 'b * 'c -> 'a
val snd' : 'a * 'b * 'c -> 'b
val thrd' : 'a * 'b * 'c -> 'c
val to_str : char -> string
val string_of_surr : surroundings -> string
val string_of_pair : int * int -> string
val string_of_dir : direction -> string
val has_neighbors : Grid.neighbors -> bool
val is_slot : Grid.board -> int -> int -> bool
val find_slots : Grid.board -> (int * int) list
val find_adj : Grid.board -> int * int -> string -> direction -> string
val get_surroundings : Grid.board -> int * int -> surroundings
val reverse_str : string -> string
val valid_chars : surroundings -> char list -> char list
val find_anchors :
  Grid.board ->
  char list -> (int * int) list -> ((int * int) * char list) list
val makes_move : direction -> surroundings -> char -> bool
val makes_prefix : direction -> surroundings -> char -> bool
val out_of_bounds : Game.state -> int * int -> bool
val is_none : 'a option -> bool
val invalid_pos : Game.state -> int * int -> bool
val get_next : direction -> int * int -> int * int
val search_next : Game.state -> direction -> int * int -> (int * int) option
val rem : 'a list -> 'a -> 'a list
val no_dups_append : 'a list -> 'a list -> 'a list
val other_dirs_move : direction -> surroundings -> char -> bool
val place_char : Game.state -> int * int -> char -> Grid.board
val valid_move :
  ('a * char list) list -> direction -> surroundings -> 'a -> char -> bool
val valid_prefix : direction -> surroundings -> char -> bool
val unpack_opt : 'a option -> 'a
val build :
  Game.state ->
  Game.player ->
  ((int * int) * char list) list ->
  int * int -> direction -> (Grid.board * ((int * int) * char) list) list
val rank_moves :
  ((int * int) * char) list list -> ((int * int) * char) list list
val pick_best : 'a list -> 'a option
val lowercase_tiles : char list -> char list
val lowercase_list : char option list -> char option list
val lowercase_grid : char option list list -> char option list list
val best_move : Game.state -> Game.player -> Game.move
val string_of_move : Game.move -> string
val simulate_game : Game.state -> unit
val run_games : 'a -> unit
