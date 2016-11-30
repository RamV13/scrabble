type point = int * int
type anchor = point * char list
type surroundings = {
  left : string;
  right : string;
  above : string;
  below : string;
}
type direction = Up | Down | Left | Right
val fst' : 'a * 'b * 'c -> 'a
val snd' : 'a * 'b * 'c -> 'b
val thrd' : 'a * 'b * 'c -> 'c
val to_str : char -> string
val list_place :
  ('a * ('b * 'c)) list -> 'b -> 'c -> 'a -> ('a * ('b * 'c)) list
val flip' : 'a * 'b -> 'b * 'a
val alphabet : char list
val has_neighbors : Grid.neighbors -> bool
val is_slot : Grid.board -> int -> int -> bool
val find_slots : Grid.board -> (int * int) list
val find_adj : Grid.board -> int * int -> string -> direction -> string
val get_surroundings : Grid.board -> int * int -> surroundings
val valid_chars : surroundings -> char list -> char list
val get_anchors :
  Grid.board ->
  char list -> (int * int) list -> ((int * int) * char list) list
val makes_move : direction -> surroundings -> char -> bool
val makes_prefix : direction -> surroundings -> char -> bool
val out_of_bounds : Game.state -> (int * int) * 'a -> bool
val get_next : direction -> (int * int) * 'a -> (int * int) * 'a
val rem : 'a list -> 'a -> 'a list
val intersect : 'a list -> 'a list -> 'a list
val print_surr : surroundings -> unit
val print_pair : int * int -> unit
val print_bool : bool -> unit
val no_dups_append : 'a list -> 'a list -> 'a list
val build :
  Game.state ->
  Game.player ->
  ((int * int) * char list) list ->
  (int * int) * 'a -> direction -> Grid.board list
