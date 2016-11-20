
open Dom
open Lwt
open Yojson

open Game

(* Yojson aliases *)

(* [from_string] is Yojson.Basic.from_string *)
let from_string = Yojson.Basic.from_string
(* [to_string_case] is Yojson.Basic.Util.to_string *)
let to_string = Yojson.Basic.Util.to_string
(* [member] is Yojson.Basic.Util.member *)
let member = Yojson.Basic.Util.member
(* [to_list] is Yojson.Basic.Util.to_list *)
let to_list = Yojson.Basic.Util.to_list
(* [to_int] is Yojson.Basic.Util.to_int *)
let to_int = Yojson.Basic.Util.to_int

(* [player_name] is the name of the player *)
let player_name = ref ""
(* [game_name] is the name of the game *)
let game_name = ref ""

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

(* [local_storage] is the localStorage javascript object *)
let local_storage = 
  match (Js.Optdef.to_option Dom_html.window##localStorage) with
  | Some value -> value
  | None -> assert false

(* [get key] gets the value associated with [key] from localStorage *)
let get key = 
  Js.Opt.get (local_storage##getItem (Js.string key)) fail |> Js.to_string

(* [get_info ()] gets the player name and game name from localStorage *)
let get_info () = 
  player_name := get "playerName";
  game_name := get "gameName"

(* [blur_current_tile ()] blurs focus on current tile by resetting its color *)
let blur_current_tile () = 
  match !current_tile with
  | Some elt -> elt##style##backgroundColor <- Js.string tile_background
  | _ -> ()

(* [get_element_by_id id] gets a DOM element by [id] *)
let get_element_by_id id = 
  Js.Opt.get Dom_html.document##getElementById (Js.string id) fail

(* [get_input_by_id id] gets a DOM input element by [id] *)
let get_input_by_id id = 
  match Dom_html.tagged (get_element_by_id id) with
  | Dom_html.Input elt -> elt
  | _ -> raise (Failure ("Element with id " ^ id ^ " is not an input"))

(* [get_tile row col] is the tile at the [row] and [col] in the board *)
let get_tile row col = 
  get_element_by_id ("grid-" ^ string_of_int row ^ "," ^ string_of_int col)

(* [handle_tile row col] is the callback to handle board tile clicks *)
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

(* [register_tiles ()] registers the callbacks for all board tiles *)
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

(* [get_player_tile row] gets the player tile at the index [row] *)
let get_player_tile row = 
  get_element_by_id ("tile-" ^ string_of_int row)

(* [handle_player_tile row] is the callback to handle player tile clicks *)
let handle_player_tile row _ = 
  blur_current_tile ();
  (get_player_tile row)##style##backgroundColor <- Js.string "#fff";
  current_tile := Some (get_player_tile row);
  current_value := Some (Js.to_string (get_player_tile row)##innerHTML);
  Js._false

(* [register_player_tiles ()] registers the callbacks for all player tiles *)
let register_player_tiles () = 
  let rec aux row = 
    if row >= 0 then
      begin
        let tile = get_player_tile row in
        tile##onclick <- Dom_html.handler (handle_player_tile row);
        aux (row - 1)
      end
  in
  aux (num_player_tiles - 1)

(* [init_state ()] initializes the UI components with the information from the 
 * game state saved in localStorage *)
let init_state () = 
  let game_state = 
    "gameState" |> get |> Yojson.Basic.from_string |> Game.from_json
  in
  let set_scoreboard_name player order = 
    let id = "scorename-" ^ (string_of_int order) in
    (get_element_by_id id)##innerHTML <- Js.string player.player_name
  in
  let count = ref 0 in
  game_state.players
  |> List.iter (fun p -> set_scoreboard_name p !count; count := !count + 1)

(* [handle_send ()] is the callback for the send chat button *)
let handle_send _ = 
  let msg = Js.to_string (get_input_by_id "message")##value in
  (get_input_by_id "message")##value <- Js.string "";
  ScrabbleClient.send_message (!player_name) (!game_name) msg;
  Js._false

(* [message_callback json] is the callback for receiving messages from others *)
let message_callback json = 
  let player_name = json |> member "playerName" |> to_string in
  let msg = json |> member "msg" |> to_string in
  let current_chat = Js.to_string ((get_element_by_id "chat")##innerHTML) in
  let new_chat = current_chat ^ "\n" ^ player_name ^ ": " ^ msg in
  (get_element_by_id "chat")##innerHTML <- Js.string new_chat

(* [contains json key] is true only if the [key] is contained in the [json] *)
let contains json key = 
  match (json |> member key) with
  | `Null -> false
  | _ -> true

(* [update_callback json] is the callback for receiving game updates *)
let update_callback json = 
  let set_scoreboard player_name order score = 
    let name_id = "scorename-" ^ (string_of_int order) in
    let score_id = "score-" ^ (string_of_int order) in
    (get_element_by_id name_id)##innerHTML <- Js.string player_name;
    (get_element_by_id score_id)##innerHTML <- Js.string (string_of_int score)
  in
  if contains json "score" then
    begin
      let player_name = json |> member "playerName" |> to_string in
      let order = json |> member "order" |> to_int in
      let score = json |> member "score" |> to_int in
      set_scoreboard player_name order score
    end

(* [onload] is the callback for when the window is loaded *)
let onload _ =
  init_state ();
  get_info ();
  register_tiles ();
  register_player_tiles ();
  (get_element_by_id "send")##onclick <- Dom_html.handler handle_send;
  ScrabbleClient.subscribe_messaging (!game_name) message_callback;
  ScrabbleClient.subscribe_updates (!game_name) update_callback;
  Js._false

let _ = 
  Dom_html.window##onload <- Dom_html.handler onload
