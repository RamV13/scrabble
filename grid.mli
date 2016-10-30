
(*a 2d list of char options represnting the board*)
type board = (char option) list list

(*my thought here was to have a map from a position (pair of coordinates) to a point value*)
type bonus_tiles = int Map.Make(int*int)


val is_empty : board -> int->int->bool
val get_tile : board -> int->int-> char option
val place : board -> int -> int -> char -> board

(*not sure if we'd implement this here or in the game logic*)
val get_neighbors : borad -> int -> int ->
  {top : char; bottom: char; left : char; right : char}