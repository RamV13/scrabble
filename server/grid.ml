
(* [board] is a 2-D list of char options representing the board *)
type board = (char option) list list

(* [bonus_tiles] is a mapping from coordinates to associated point values *)
type bonus_tiles = ((int * int) * int) list

(* [empty] is the empty board *)
let empty = []

(* [is_empty board row col] is true if the coordinate ([row],[col] is empty in 
 * the [board] *)
let is_empty board row col = 
  false (* TODO *)

(* [get_tile board row col] is a char option representing the tile at the 
 * coordinate ([row],[col]) in the [board] *)
let get_tile board row col = 
  None (* TODO *)

(* [place board row col value] is a new board with the character [value] placed
 * at the coordinate ([row],[col]) in the [board] *)
let place board row col value = 
  board (* TODO *)

(* [neighbors] contains a set of four tiles adjacent to a central tile *)
type neighbors = {
  top : char option;
  bottom : char option;
  left : char option;
  right : char option;
}

(* [get_neighbors board row col] is the set of four tiles adjacent to the tile
 * at the coordinate ([row],[col]) in the [board] *)
let get_neighbors board row col = 
  {top=None;bottom=None;left=None;right=None} (* TODO *)
