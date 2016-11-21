
open Dom
open Lwt
open ScrabbleClient

(* [fail] is a failure callback *)
let fail = fun _ -> assert false

(* [get_element_by_id id] gets a DOM element by [id] *)
let get_element_by_id id = 
  Js.Opt.get Dom_html.document##getElementById (Js.string id) fail

(* [get_input_by_id id] gets a DOM input element by [id] *)
let get_input_by_id id = 
  match Dom_html.tagged (get_element_by_id id) with
  | Dom_html.Input elt -> elt
  | _ -> raise (Failure ("Element with id " ^ id ^ " is not an input"))

(* [local_storage] is the localStorage javascript object *)
let local_storage = 
  match (Js.Optdef.to_option Dom_html.window##localStorage) with
  | Some value -> value
  | None -> assert false

(* [save_info player_name game_name] saves the [player_name] and [game_name] to
 * localStorage *)
let save_info player_name game_name = 
  local_storage##setItem (Js.string "playerName",Js.string player_name);
  local_storage##setItem (Js.string "gameName",Js.string game_name)

(* [save_game game_state] saves the game state [game_state] in local storage *)
let save_game game_state = 
  let json = Game.to_json game_state in
  local_storage##setItem (Js.string "gameState",Js.string json)

(* [handle_btn_join btn ()] is the callback to handle the click events of the 
 * join button [btn] *)
let handle_btn_join btn _ = 
  let player_name = Js.to_string (get_input_by_id "text_name")##value in
  let game_name = Js.to_string (get_input_by_id "text_game")##value in
  ScrabbleClient.join_game player_name game_name >>= (fun result ->
    begin
      (match result with
      | Val state -> 
        begin
          save_info player_name game_name;
          save_game state;
          Dom_html.window##location##href <- Js.string "scrabble.html"
        end
      | Not_found msg -> Dom_html.window##alert (Js.string msg)
      | Full msg -> Dom_html.window##alert (Js.string msg)
      | Exists msg -> Dom_html.window##alert (Js.string msg)
      | Server_error msg -> Dom_html.window##alert (Js.string msg));
      return ()
    end)
  |> ignore;
  Js._false

(* [handle_btn_create btn ()] is the callback to handle the click events of the 
 * create button [btn] *)
let handle_btn_create btn _ = 
  let player_name = Js.to_string (get_input_by_id "text_name")##value in
  let game_name = Js.to_string (get_input_by_id "text_game")##value in
  ScrabbleClient.create_game player_name game_name >>= (fun result ->
    begin
      (match result with
      | Val state ->
        begin
          save_info player_name game_name;
          save_game state;
          Dom_html.window##location##href <- Js.string "scrabble.html"
        end
      | Exists msg -> Dom_html.window##alert (Js.string msg)
      | Server_error msg -> Dom_html.window##alert (Js.string msg)
      | _ -> assert false
      );
      Lwt.return ()
    end)
  |> ignore;
  Js._false

(* [onload ()] is the callback for when the window is loaded *)
let onload _ =
  let btn_join = get_element_by_id "btn_join" in
  let btn_create = get_element_by_id "btn_create" in
  btn_join##onclick <- Dom_html.handler (handle_btn_join btn_join);
  btn_create##onclick <- Dom_html.handler (handle_btn_create btn_create);
  Js._false

let _ = 
  Dom_html.window##onload <- Dom_html.handler onload
