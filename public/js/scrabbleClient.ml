
open Lwt
open Cohttp
open Yojson
open XHRClient

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

let baseURL = "http://127.0.0.1" (* "http://128.253.51.200" *)

let event_source_constructor = Js.Unsafe.global##_EventSource

(* [headers] is the default headers for JSON requests *)
let headers = Header.init_with "content-type" "application/json"

(* [result] contains the resulting value from a request with possible errors *)
type 'a result = Val of 'a 
                 | Exists of string
                 | Not_found of string 
                 | Full of string 
                 | Server_error of string

(* [server_error_msg] is the message corresponding to server errors *)
let server_error_msg =
  "Something went wrong. Please ensure that the server is running properly."

let empty_state =
  {
    name = "";
    grid = Grid.empty;
    players = [];
    remaining_tiles = [];
    turn = 0
  }

let join_game player_name game_name = 
  {
    headers;
    meth = `POST;
    url = baseURL ^ "/api/game";
    req_body = "{\"playerName\":\"" ^ player_name ^ "\", \"gameName\":\"" ^ 
                game_name ^ "\"}"
  }
  |> XHRClient.exec
  >>= fun res -> 
      begin
        (
        match res.status with
        | `OK -> Val (Game.from_json (Yojson.Basic.from_string res.res_body))
        | `Not_found -> Not_found res.res_body
        | `Bad_request -> Full res.res_body
        | `Not_acceptable -> Exists res.res_body
        | _ -> Server_error server_error_msg
        )
        |> Lwt.return
      end

let create_game player_name game_name = 
  {
    headers;
    meth = `PUT;
    url = baseURL ^ "/api/game";
    req_body = "{\"playerName\":\"" ^ player_name ^ "\", \"gameName\":\"" ^ 
                game_name ^ "\"}"
  }
  |> XHRClient.exec
  >>= fun res -> 
      begin
        (
        match res.status with
        | `OK -> Val (Game.from_json (Yojson.Basic.from_string res.res_body))
        | `Bad_request -> Exists res.res_body
        | _ -> Server_error server_error_msg
        )
        |> Lwt.return
      end

let leave_game player_name game_name = 
  {
    headers;
    meth = `DELETE;
    url = baseURL ^ "/api/game";
    req_body = "{\"playerName\":\"" ^ player_name ^ "\", \"gameName\":\"" ^ 
                game_name ^ "\"}"
  }
  |> XHRClient.exec
  |> ignore

let execute_move id move = 
  () (* TODO *)

(* [subscribe endpoint game_name callback] subscribes a callback to a event 
 * source of the game with name [game_name] associated with an endpoint *)
let subscribe endpoint game_name callback = 
  let base = Uri.of_string (baseURL ^ "/api/" ^ endpoint) in
  let url = Uri.with_query base [("gameName",[game_name])] |> Uri.to_string in
  let event_source = jsnew event_source_constructor (Js.string url) in
  event_source##onmessage <- Js.wrap_callback (fun event ->
    event##data
    |> Js.to_string
    |> Yojson.Basic.from_string
    |> callback
  );
  event_source##onerror <- Js.wrap_callback (fun event -> 
    Dom_html.window##alert (event##data);
    event_source##close ()
  )

let subscribe_updates = 
  subscribe "game"

let send_message player_name game_name msg = 
  {
    headers;
    meth = `POST;
    url = baseURL ^ "/api/messaging";
    req_body = "{\"playerName\":\"" ^ player_name ^ "\", \"gameName\":\"" ^ 
                game_name ^ "\", \"msg\":\"" ^ msg ^ "\"}"
  }
  |> XHRClient.exec
  |> ignore

let subscribe_messaging = 
  subscribe "messaging"
