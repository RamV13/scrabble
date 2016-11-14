
open Dom

open Lwt

(* [board_id] is the HTML id for the board *)
let board_id = "board"

(* [board_dimension] is the dimension of the board *)
let board_dimension = 15

(* [num_player_tiles] is the number of player tiles *)
let num_player_tiles = 7

(* [tile_background] is the value of the tile background color *)
let tile_background = "#EFEBE9"

(* [dark_tile_background] is the value of the dark tile background color *)
let dark_tile_background = "#D7CCC8"

(* [current_tile] is the current focused tile *)
let current_tile : Dom_html.element Js.t option ref = ref None

(* [current_value] is the current tile value about to be placed *)
let current_value : string option ref = ref None

(* [fail] is a failure callback *)
let fail = fun _ -> assert false

let blur_current_tile _ = 
  match !current_tile with
  | Some elt -> elt##style##backgroundColor <- Js.string tile_background
  | _ -> ()

(* [get_element_by_id id] gets a DOM element by [id] *)
let get_element_by_id id = 
  Js.Opt.get Dom_html.document##getElementById (Js.string id) fail

(* [get_tile row col] is the tile at the [row] and [col] in the board *)
let get_tile row col = 
  get_element_by_id ("grid-" ^ string_of_int row ^ "," ^ string_of_int col)

let handle_tile row col _ = 
  (match !current_value with
  | Some value -> 
    begin
      let tile = get_tile row col in
      let not_bonus = 
        let regex = Regexp.regexp ".*W|L|★" in
        match Regexp.string_match regex (Js.to_string tile##innerHTML) 0 with
        | None -> true
        | _ -> false
      in
      if not_bonus 
      then tile##style##backgroundColor <- Js.string dark_tile_background;
      tile##innerHTML <- Js.string value
    end
  | _ -> ());
  blur_current_tile ();
  (* TODO remove used tile and replace with new tile *)
  Js._false

let register_tiles () = 
  let rec aux row col = 
    if row >= 0 && col >= 0 then
      begin
        (get_tile row col)##onclick <- Dom_html.handler (handle_tile row col);
        if col = 0 then aux (row - 1) (board_dimension - 1)
        else aux row (col - 1)
      end
  in
  aux (board_dimension - 1) (board_dimension - 1)

let get_player_tile row = 
  get_element_by_id ("tile-" ^ string_of_int row)

let handle_player_tile row _ = 
  blur_current_tile ();
  (get_player_tile row)##style##backgroundColor <- Js.string "#fff";
  current_tile := Some (get_player_tile row);
  current_value := Some (Js.to_string (get_player_tile row)##innerHTML);
  Js._false

let register_player_tiles () = 
  let rec aux row = 
    if row >= 0 then
      begin
        (get_player_tile row)##onclick <- Dom_html.handler (handle_player_tile row);
        aux (row - 1)
      end
  in
  aux (num_player_tiles - 1)

let onload _ =
  register_tiles ();
  register_player_tiles ();
  Js._false

let _ = 
  Dom_html.window##onload <- Dom_html.handler onload
