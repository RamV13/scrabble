
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

(* [cur_player] is the current player *)
let cur_player = ref {player_name="";tiles=[];score=0;order=0;ai=false}
(* [player_name] is the name of the player *)
let player_name = ref ""
(* [game_name] is the name of the game *)
let game_name = ref ""

(* [loaded] is a boolean flag to indicate if the page is fresh *)
let loaded = ref false

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
(* [placed_tiles] is the current list of placed tiles as an association list *)
let placed_tiles : ((int * int) * char) list ref = ref []

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

let set key value = 
  local_storage##setItem (Js.string key,Js.string value)

(* [remove key] removes the pair associated with [key] from localStorage *)
let remove key = 
  local_storage##removeItem (Js.string key)

(* [get_info ()] gets the player name and game name from localStorage *)
let get_info () = 
  player_name := get "playerName";
  game_name := get "gameName"

(* [current_value ()] gets the current value of the current tile if selected *)
let current_value () = 
  match !current_tile with
  | Some elt -> Some (Js.to_string elt##innerHTML)
  | _ -> None

(* [blur_current_tile ()] blurs focus on the current selected player tile *)
let blur_current_tile () =
  match !current_tile with
  | Some elt -> elt##style##backgroundColor <- Js.string tile_background
  | _ -> ()

(* [reset_current_tile ()] resets the current tile *)
let reset_current_tile () = 
  match !current_tile with
  | Some elt -> 
    begin
      blur_current_tile (); 
      elt##innerHTML <- Js.string "";
      current_tile := None
    end
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
  (match current_value () with
  | Some value -> 
    begin
      let tile = get_tile row col in
      let not_bonus = 
        let regex = Regexp.regexp ".*W|L|★" in
        match Regexp.string_match regex (Js.to_string tile##innerHTML) 0 with
        | None -> true
        | _ -> false
      in
      let filled = String.length (Js.to_string tile##innerHTML) = 1 in
      if not filled then
        begin
          if not_bonus
          then tile##style##backgroundColor <- Js.string dark_tile_background;
          tile##innerHTML <- Js.string value;
          placed_tiles := ((row,col),value.[0])::!placed_tiles;
          reset_current_tile ()
        end
    end
  | _ -> ());
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
  let new_value = Js.to_string (get_player_tile row)##innerHTML in
  if new_value <> "" then
    begin
      (get_player_tile row)##style##backgroundColor <- Js.string "#fff";
      current_tile := Some (get_player_tile row)
    end;
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

(* [reset_player_tiles ()] resets player tiles based on the current player *)
let reset_player_tiles () = 
  let reset_tile row col = 
    let tile = get_tile row col in
    let not_bonus = 
      (Js.to_string tile##style##backgroundColor) = "rgb(215, 204, 200)"
    in
    if not_bonus
    then tile##style##backgroundColor <- Js.string tile_background;
    tile##innerHTML <- Js.string "&nbsp;"
  in
  List.iter (fun ((row,col),_) -> reset_tile row col) !placed_tiles;
  placed_tiles := [];
  let rec aux row = 
    if row >= 0 then
      begin
        let tile = get_player_tile row in
        (* TODO retrieve from real players' tiles *)
        let value = "A" in (* Char.escaped (List.nth (!cur_player).tiles row) in *)
        tile##innerHTML <- Js.string value;
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
  remove "gameState";
  game_state.players
  |> List.iter (fun p -> set_scoreboard_name p !count; count := !count + 1);
  let player = 
    game_state.players
    |> List.find (fun player -> player.player_name = (!player_name))
  in
  cur_player := player;
  reset_player_tiles ()

(* [handle_submit ()] is the callback for the submit button of the game *)
let handle_submit _ = 
  (* TODO send up diff tiles from `placed_tiles` *)
  Js._false

(* [handle_reset ()] is the callback for the reset button of the game *)
let handle_reset _ = 
  reset_player_tiles ();
  Js._false

(* [handle_send ()] is the callback for the send chat button *)
let handle_send _ = 
  let input = get_input_by_id "message" in
  let json = Js.to_string (Json.output input##value) in
  let msg = String.sub json 1 ((String.length json) - 2) in
  input##value <- Js.string "";
  input##focus ();
  if msg <> "" then ScrabbleClient.send_message (!player_name) (!game_name) msg;
  Js._false

let handle_input event = 
  if (event##keyCode = 13) then handle_send ()
  else Js._false

(* [handle_message json] is the callback for receiving messages from others *)
let handle_message json = 
  let player_name = json |> member "playerName" |> to_string in
  let msg = json |> member "msg" |> to_string in
  let current_chat = Js.to_string ((get_element_by_id "chat")##innerHTML) in
  let new_chat = current_chat ^ "\n" ^ player_name ^ ": " ^ msg in
  let chat_window = get_element_by_id "chat" in
  chat_window##innerHTML <- Js.string new_chat;
  chat_window##scrollTop <- chat_window##scrollHeight

(* [contains json key] is true only if the [key] is contained in the [json] *)
let contains json key = 
  match (json |> member key) with
  | `Null -> false
  | _ -> true

(* [handle_update json] is the callback for receiving game updates *)
let handle_update json = 
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
  (try
    get_info ();
    init_state ();
    remove "playerName";
    remove "gameName";
    loaded := true
  with _ -> Dom_html.window##location##href <- Js.string "index.html");
  register_tiles ();
  register_player_tiles ();
  (get_element_by_id "submit")##onclick <- Dom_html.handler handle_submit;
  (get_element_by_id "reset")##onclick <- Dom_html.handler handle_reset;
  (get_element_by_id "send")##onclick <- Dom_html.handler handle_send;
  (get_element_by_id "message")##onkeyup <- Dom_html.handler handle_input;
  ScrabbleClient.subscribe_messaging (!player_name) (!game_name) handle_message;
  ScrabbleClient.subscribe_updates (!player_name) (!game_name) handle_update;
  Js._false

(* [onunload] is the callback for when the window is about to be leaved *)
let onunload _ = 
  if !loaded then
    begin
      ScrabbleClient.leave_game (!player_name) (!game_name);
      ScrabbleClient.close_sources ();
      loaded := false
    end;
  Js._false

let _ = 
  Dom_html.window##onload <- Dom_html.handler onload;
  Dom_html.window##onunload <- Dom_html.handler onunload
