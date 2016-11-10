(* [board] is a 2-D list of char options representing the board *)
type board = (char option) list list

(* [bonus_tiles] is an association list from coordinates to point values *)
val bonus_tiles : ((int * int) * int) list

(* [empty] is the empty board *)
val empty : board

(* [is_empty board row col] is true if the coordinate ([row],[col] is empty in
 * the [board] *)
val is_empty : board -> int -> int -> bool

(* [get_tile board row col] is a char option representing the tile at the
 * coordinate ([row],[col]) in the [board] *)
val get_tile : board -> int -> int -> char option

(* [place board row col value] is a new board with the character [value] placed
 * at the coordinate ([row],[col]) in the [board] *)
val place : board -> int -> int -> char -> board

(* [neighbors] contains a set of four tiles adjacent to a central tile *)
type neighbors = {
  top : char option;
  bottom : char option;
  left : char option;
  right : char option;
}

(* [get_neighbors board row col] is the set of four tiles adjacent to the tile
 * at the coordinate ([row],[col]) in the [board] *)
val get_neighbors : board -> int -> int -> neighbors
