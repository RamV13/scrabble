
open Dom
open Lwt
open ScrabbleClient
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
(* [turn] is the current turn of the game *)
let turn = ref 0

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
(* [score_name_color] is the value of the highlighted score name color *)
let score_name_color = "#EF5350"
(* [double_letter_background] is the background color of double letter tiles *)
let double_letter_background = "#81D4FA"
(* [double_word_background] is the background color of double word tiles *)
let double_word_background = "#EF9A9A"
(* [triple_letter_background] is the background color of triple letter tiles *)
let triple_letter_background = "#039BE5"
(* [triple_word_background] is the background color of triple word tiles *)
let triple_word_background = "#EF5350"

(* [drag_value] is the current dragged value *)
let drag_value : string ref = ref ""
(* [dragging] is a boolean flag indicating a drag in progress *)
let dragging = ref false
(* [current_tile] is the current focused tile *)
let current_tile : Dom_html.element Js.t option ref = ref None
(* [replace_tiles] is the current focused tiles *)
let replace_tiles : Dom_html.element Js.t list ref = ref []
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

(* [blur_current_tiles ()] blurs focus on the current selected player tiles *)
let blur_current_tiles () =
  let blur_tile tile = 
    tile##style##backgroundColor <- Js.string tile_background
  in
  List.iter blur_tile !replace_tiles

(* [reset_current_tile ()] resets the current tile *)
let reset_current_tile () = 
  match !current_tile with
  | Some elt -> 
    begin
      blur_current_tiles (); 
      elt##innerHTML <- Js.string "";
      current_tile := None
    end
  | _ -> ()

(* [notify msg] displays a snackbar notification with a [msg] body *)
let notify msg = 
  " var notification = document.querySelector('.mdl-js-snackbar');
    notification.MaterialSnackbar.showSnackbar({message: \"" ^ msg ^ "\"});
  "
  |> Js.Unsafe.eval_string
  |> ignore

(* [dialog col] displays a dialog for selecting a wildcard tile letter value *)
let dialog col = 
  " var dialog = document.querySelector('dialog');
    dialog.showModal();
    var current = null;

    for (i = 0; i < 26; i++) {
      (function() {
        var id = 'letter-' + String.fromCharCode(97 + i);
        var letter = document.getElementById(id);
        letter.addEventListener('click', function() {
          if (current) {
            current.style.color = '#000';
            current.style.backgroundColor = '#fff';
          }
          letter.style.color = '#009688';
          letter.style.backgroundColor = 'lightgrey';
          current = letter;
        });
      }());
    }

    function resetCurrent() {
      if (current) {
        current.style.color = '#000';
        current.style.backgroundColor = '#fff';
        current = null;
      }
    }

    function handleSelect() {
      if (current) {
        var playerTiles = [];
        for (i = 0; i < " ^ string_of_int num_player_tiles ^ "; i++) {
          playerTiles.push(document.getElementById('tile-' + i).innerHTML);
        }
        if (playerTiles.includes(current.innerHTML)) {
          var notification = document.querySelector('.mdl-js-snackbar');
          notification.MaterialSnackbar.showSnackbar({message: 
            \"You already have a '\" + current.innerHTML + 
            \"' tile. Please place this first.\"});
          resetCurrent();
          document.getElementById('select').
            removeEventListener('click', handleSelect);
          dialog.close();
        } else {
          var id = 'tile-" ^ string_of_int col ^ "';
          document.getElementById(id).innerHTML = current.innerHTML;
          document.getElementById(id).draggable = true;
          resetCurrent();
          document.getElementById('select').
            removeEventListener('click', handleSelect);
          dialog.close();
        }
      }
    }

    document.getElementById('select').addEventListener('click', handleSelect);
    dialog.querySelector('.close').addEventListener('click', function() {
      resetCurrent();
      dialog.close();
    });
  "
  |> Js.Unsafe.eval_string
  |> ignore

(* [reflow row col] triggers a reflow on the tile at [row] and [col] *)
let reflow row col = 
  let coords = string_of_int row ^ "," ^ string_of_int col in
  "document.getElementById('grid-" ^ coords ^ "').offsetHeight;"
  |> Js.Unsafe.eval_string
  |> ignore

(* [get_element_by_id id] gets a DOM element by [id] *)
let get_element_by_id id = 
  Js.Opt.get Dom_html.document##getElementById (Js.string id) fail

(* [get_input_by_id id] gets a DOM input element by [id] *)
let get_input_by_id id = 
  match Dom_html.tagged (get_element_by_id id) with
  | Dom_html.Input elt -> elt
  | _ -> raise (Failure ("Element with id " ^ id ^ " is not an input"))

(* [get_button_by_id id] gets a DOM button element by [id] *)
let get_button_by_id id = 
  match Dom_html.tagged (get_element_by_id id) with
  | Dom_html.Button elt -> elt
  | _ -> raise (Failure ("Element with id " ^ id ^ " is not a button"))

(* [enable_controls ()] enables the "Submit", "Reset", and tile buttons *)
let enable_controls () = 
  (get_button_by_id "submit")##disabled <- Js._false;
  (get_button_by_id "reset")##disabled <- Js._false;
  (get_button_by_id "pass")##disabled <- Js._false;
  (get_button_by_id "replace")##disabled <- Js._false

(* [disable_controls ()] disables the "Submit", "Reset", and tile buttons *)
let disable_controls () = 
  (get_button_by_id "submit")##disabled <- Js._true;
  (get_button_by_id "reset")##disabled <- Js._true;
  (get_button_by_id "pass")##disabled <- Js._true;
  (get_button_by_id "replace")##disabled <- Js._true

(* [highlight_score_name order] highlights the score name at [order] *)
let highlight_score_name order = 
  let id = "scorename-" ^ (string_of_int order) in
  (get_element_by_id id)##style##color <- Js.string score_name_color

(* [reset_score_name order] resets the score name at [order] *)
let reset_score_name order = 
  let id = "scorename-" ^ (string_of_int order) in
  (get_element_by_id id)##style##color <- Js.string "#000"

(* [get_tile row col] is the tile at the [row] and [col] in the board *)
let get_tile row col = 
  get_element_by_id ("grid-" ^ string_of_int row ^ "," ^ string_of_int col)

(* [place_tile row col value] places a letter [value] on the board tile at the
 * [row] and [col] coordinates *)
let place_tile row col value = 
  let tile = get_tile row col in
  let filled = String.length (Js.to_string tile##innerHTML) = 1 in
  if not filled then
    begin
      tile##className <- Js.string "scrabble-td no-transition";
      tile##style##color <- Js.string "rgba(0, 0, 0, 0)";
      reflow row col;
      tile##className <- Js.string "scrabble-td";
      tile##style##backgroundColor <- Js.string dark_tile_background;
      tile##style##color <- Js.string "rgba(0, 0, 0, 1)";
      tile##innerHTML <- Js.string value;
      let upper = value.[0] |> Char.uppercase_ascii in
      placed_tiles := ((row,col),upper)::!placed_tiles;
      reset_current_tile ()
    end

(* [handle_tile row col] is the callback to handle board tile clicks *)
let handle_tile row col _ = 
  (match current_value () with
  | Some value -> place_tile row col value
  | _ -> ());
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

(* [get_player_tile col] gets the player tile at the index [col] *)
let get_player_tile col = 
  get_element_by_id ("tile-" ^ string_of_int col)

(* [handle_player_tile col] is the callback to handle player tile clicks *)
let handle_player_tile col _ = 
  if (!cur_player).order = !turn then
    begin
      let new_value = Js.to_string (get_player_tile col)##innerHTML in
      if new_value = "?" then dialog col
      else if new_value <> "" then
        begin
          let tile = get_player_tile col in
          if List.mem tile !replace_tiles then
            begin
              tile##style##backgroundColor <- Js.string tile_background;
              replace_tiles := 
                !replace_tiles
                |> List.filter (fun tile' -> tile <> tile')
            end
          else
            begin
              tile##style##backgroundColor <- Js.string "#fff";
              replace_tiles := tile::!replace_tiles
            end
        end
    end;
  Js._false

(* [register_player_tiles ()] registers the callbacks for all player tiles *)
let register_player_tiles () = 
  let rec aux col = 
    if col >= 0 then
      begin
        let tile = get_player_tile col in
        tile##onclick <- Dom_html.handler (handle_player_tile col);
        aux (col - 1)
      end
  in
  aux (num_player_tiles - 1)

(* [reset_tile row col] resets the tile at [row] and [col] *)
let reset_tile row col = 
  let tile = get_tile row col in
  let check_tile ((r,c),_) = r = row && c = col in
  let is_bonus = 
    List.exists check_tile Grid.bonus_letter_tiles ||
    List.exists check_tile Grid.bonus_word_tiles
  in
  if not is_bonus then
    begin
      tile##style##backgroundColor <- Js.string tile_background;
      tile##innerHTML <- Js.string "&nbsp;"
    end
  else
    begin
      let second = 
        if List.exists check_tile Grid.bonus_letter_tiles then "L" else "W"
      in
      let bonus = 
        List.filter check_tile Grid.bonus_letter_tiles @
        List.filter check_tile Grid.bonus_word_tiles
        |> List.hd
        |> snd
        |> string_of_int
      in
      if row = (board_dimension - 1) / 2 && col = (board_dimension - 1) / 2
      then 
        begin
          tile##innerHTML <- Js.string "★";
          tile##style##backgroundColor <- Js.string double_word_background
        end
      else 
        begin
          let value = bonus ^ second in
          tile##innerHTML <- Js.string value;
          if value = "2L" then 
            tile##style##backgroundColor <- Js.string double_letter_background
          else if value = "2W" then 
            tile##style##backgroundColor <- Js.string double_word_background
          else if value = "3L" then 
            tile##style##backgroundColor <- Js.string triple_letter_background
          else if value = "3W" then 
            tile##style##backgroundColor <- Js.string triple_word_background
          else assert false
        end
    end

(* [reset_player_tiles ()] resets player tiles based on the current player *)
let reset_player_tiles () = 
  blur_current_tiles ();
  List.iter (fun ((row,col),_) -> reset_tile row col) !placed_tiles;
  placed_tiles := [];
  replace_tiles := [];
  let rec aux col = 
    if col >= 0 then
      begin
        let tile = get_player_tile col in
        let value = 
          List.nth (!cur_player).tiles col
          |> Char.uppercase_ascii
          |> Char.escaped
        in
        tile##innerHTML <- Js.string value;
        tile##setAttribute (Js.string "draggable",Js.string "true");
        tile##ondragstart <- Dom_html.handler (fun _ ->
          let content = Js.to_string tile##innerHTML in
          if content = "?" || content = "" || (!cur_player).order <> !turn
          then Js._false
          else
            begin
              blur_current_tiles ();
              drag_value := content;
              dragging := true;
              current_tile := Some tile;
              Js._true
            end
        );
        tile##ondragend <- Dom_html.handler (fun _ ->
          if !dragging then tile##innerHTML <- Js.string !drag_value;
          Js._false
        );
        tile##ondrag <- Dom_html.handler (fun _ ->
          tile##innerHTML <- Js.string "&nbsp;";
          Js._false
        );
        tile##ondragover <- Dom_html.handler (fun _ -> Js._false);
        tile##ondrop <- Dom_html.handler (fun _ ->
          let content = Js.to_string tile##innerHTML in
          if content = "" then
            begin
              dragging := false;
              tile##innerHTML <- Js.string !drag_value
            end;
          Js._false
        );
        aux (col - 1)
      end
  in
  aux (num_player_tiles - 1)

(* [init_state ()] initializes the UI components with the information from the 
 * game state saved in localStorage *)
let init_state () = 
  let game_state = 
    "gameState" |> get |> Yojson.Basic.from_string |> Game.state_from_json
  in
  remove "gameState";
  let set_scoreboard player = 
    let name_id = "scorename-" ^ (string_of_int player.order) in
    let score_id = "score-" ^ (string_of_int player.order) in
    (get_element_by_id name_id)##innerHTML <- Js.string player.player_name;
    let score_string = string_of_int player.score in
    (get_element_by_id score_id)##innerHTML <- Js.string score_string
  in
  let x = ref 0 in
  let y = ref 0 in
  let update_tile grid_value = 
    (match grid_value with
    | Some value -> 
      begin
        value
        |> Char.uppercase_ascii
        |> Char.escaped
        |> place_tile !y !x
      end
    | None -> ());
    let tile = get_tile !y !x in
    let x' = !x in
    let y' = !y in
    tile##setAttribute (Js.string "draggable",Js.string "true");
    tile##ondragstart <- Dom_html.handler (fun _ ->
      let placed_tile = 
        let check_tile = fun acc ((y,x),_) -> (y = y' && x = x') || acc in
        List.fold_left check_tile false !placed_tiles
      in
      if placed_tile && (!cur_player).order = !turn then 
        begin
          placed_tiles := 
            !placed_tiles
            |> List.filter (fun ((y,x),_) -> y <> y' || x <> x');
          dragging := true;
          drag_value := Js.to_string tile##innerHTML;
          Js._true
        end
      else Js._false
    );
    tile##ondrag <- Dom_html.handler (fun _ ->
      reset_tile y' x';
      Js._false
    );
    tile##ondragend <- Dom_html.handler (fun _ ->
      if !dragging then 
        begin
          tile##innerHTML <- Js.string !drag_value;
          tile##style##backgroundColor <- Js.string dark_tile_background;
          placed_tiles := ((y',x'),(!drag_value).[0])::!placed_tiles
        end;
      Js._false
    );
    tile##ondragover <- Dom_html.handler (fun _ -> Js._false);
    tile##ondrop <- Dom_html.handler (fun _ ->
      let filled = String.length (Js.to_string tile##innerHTML) = 1 in
      if not filled then 
        begin
          dragging := false;
          blur_current_tiles ();
          let start = 5 in
          let id = (Js.to_string tile##id) in
          let comma_index = String.index id ',' in
          let offset = comma_index - start in
          let end_length = (String.length id) - (start + offset + 1) in
          let row_str = String.sub id start offset in
          let col_str = String.sub id (start + offset + 1) end_length in
          let row = int_of_string row_str in
          let col = int_of_string col_str in
          place_tile row col !drag_value
        end;
      Js._false
    );
    x := 1 + !x
  in
  game_state.grid
  |> List.iter (fun row -> List.iter update_tile row; y := 1 + !y; x := 0);
  placed_tiles := [];
  List.iter (fun player -> set_scoreboard player) game_state.players;
  let player = 
    game_state.players
    |> List.find (fun player -> player.player_name = (!player_name))
  in
  cur_player := player;
  turn := game_state.turn;
  highlight_score_name game_state.turn;
  if player.order <> game_state.turn then disable_controls ();
  reset_player_tiles ()

(* [execute move] performs the HTTP requests for executive a move and effects
 * the corresponding GUI changes *)
let execute move = 
  ScrabbleClient.execute_move (!game_name) move >>= (fun result ->
    begin
      (
        match result with
        | Success -> ()
        | Failed msg -> notify msg
        | Server_error msg -> Dom_html.window##alert (Js.string msg)
        | _ -> assert false
      );
      Lwt.return ()
    end
  )
  |> ignore;
  reset_player_tiles ()

(* [handle_place ()] is the callback for the place button of the game *)
let handle_place _ = 
  if List.length !placed_tiles > 0 then
    begin
      {tiles_placed=(!placed_tiles);player=(!cur_player).player_name;swap=[]}
      |> execute
    end
  else notify "No tiles placed!";
  Js._false

(* [handle_reset ()] is the callback for the reset button of the game *)
let handle_reset _ = 
  reset_player_tiles ();
  Js._false

(* [handle_pass ()] is the callback for the pass button of the game *)
let handle_pass _ = 
  execute {tiles_placed=[];player=(!cur_player).player_name;swap=[]};
  Js._false

(* [handle_replace ()] is the callback for the replace button of the game *)
let handle_replace _ = 
  let swap = 
    !replace_tiles
    |> List.fold_left (fun acc elt -> (Js.to_string elt##innerHTML).[0]::acc) []
  in
  if List.length swap = 0 then notify "No tiles selected!"
  else execute {tiles_placed=[];player=(!cur_player).player_name;swap};
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
  match member key json with
  | `Null -> false
  | _ -> true

(* [handle_update json] is the callback for receiving game updates *)
let handle_update json = 
  if contains json "order" then
    begin
      let add_new_player player_name order = 
        let name_id = "scorename-" ^ (string_of_int order) in
        let prev_name = Js.to_string (get_element_by_id name_id)##innerHTML in
        (get_element_by_id name_id)##innerHTML <- Js.string player_name;
        notify (prev_name ^ " left, " ^ player_name ^ " joined")
      in
      let player_name = json|> member "playerName" |> to_string in
      let order = json |> member "order" |> to_int in
      add_new_player player_name order
    end
  else if contains json "board_diff" then
    begin
      let diff = Game.diff_from_json json in
      let player = List.hd diff.players_diff in
      let update_tile ((row,col),value) = 
        place_tile row col (Char.escaped value)
      in
      List.iter update_tile diff.board_diff;
      placed_tiles := [];
      let set_scoreboard order score = 
        let score_id = "score-" ^ (string_of_int order) in
        (get_element_by_id score_id)##innerHTML <- Js.string (string_of_int score)
      in
      set_scoreboard player.order player.score;
      if (!cur_player).order = player.order then 
        begin
          cur_player := player;
          reset_player_tiles ()
        end;
      reset_score_name !turn;
      highlight_score_name diff.new_turn_val;
      turn := diff.new_turn_val;
      if (!cur_player).order <> !turn then disable_controls () 
      else
        begin
          enable_controls ();
          notify "It's your turn!"
        end
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
  (get_element_by_id "submit")##onclick <- Dom_html.handler handle_place;
  (get_element_by_id "reset")##onclick <- Dom_html.handler handle_reset;
  (get_element_by_id "pass")##onclick <- Dom_html.handler handle_pass;
  (get_element_by_id "replace")##onclick <- Dom_html.handler handle_replace;
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
