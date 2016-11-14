
open Dom

open Lwt

(* [board_id] is the HTML id for the board *)
let board_id = "board"

(* [fail] is a failure callback *)
let fail = fun _ -> assert false

(* [get_element_by_id id] gets a DOM element by [id] *)
let get_element_by_id id = 
  Js.Opt.get Dom_html.document##getElementById (Js.string id) fail

(* [get_tile row col] is the tile at the [row] and [col] in the board *)
let get_tile row col = 
  get_element_by_id ("grid-" ^ string_of_int row ^ "," ^ string_of_int col)

let onload _ =
  Js._false

let _ = 
  Dom_html.window##onload <- Dom_html.handler onload
