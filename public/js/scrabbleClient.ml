
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
        | `OK -> Val empty_state
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
        | `OK -> Val empty_state
        | `Bad_request -> Exists res.res_body
        | _ -> Server_error server_error_msg
        )
        |> Lwt.return
      end

let get_game_state id = 
  Lwt.return (Val empty_state) (* TODO *)

let execute_move id move = 
  Lwt.return (Val empty_state) (* TODO *)

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
