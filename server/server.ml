
open Cohttp
open Cohttp_lwt_unix
open Yojson
open HttpServer
open Lwt

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

(* [games] is the list of games currently running *)
let games = ref []
(* [game_pushers] is an association list from games to their respective game
 * related update clients *)
let game_pushers = ref []
(* [msg_pushers] is an association list from games to their respective message
 * clients *)
let msg_pushers = ref []

(* [origin] is the allowed origin to request this server *)
let origin = "*"

(* [default_headers] are the set of default headers for plain text responses *)
let default_headers =
  Header.init_with "content-type" "text/plain"
  |> fun header -> Header.add header "Access-Control-Allow-Origin" origin

(* [headers] are the default set of headers for JSON responses *)
let headers = 
  Header.init_with "content-type" "application/json"
  |> fun header -> Header.add header "Access-Control-Allow-Origin" origin

(* [server_error_msg] is the default message for extraneous server errors *)
let server_error_msg = 
  "Something went wrong. Please check the status of the server"

(* [cors_control req] responds with Access-Control headers to enable CORS *)
let cors_control req = 
  let headers = 
    Header.init_with "Access-Control-Allow-Origin" origin 
    |> fun header -> Header.add header "Access-Control-Allow-Headers" 
                                       "content-type"
    |> fun header -> Header.add header "Access-Control-Allow-Methods"
                                       "GET,POST,PUT,DELETE,OPTIONS"
  in
  {headers;status=`OK;res_body=""}

(* [send main_pushers game_name sendable] sends an SSE with a [sendable] payload
 * to all valid clients of the game [game_name] in the [main_pushers] list *)
let send main_pushers game_name sendable = 
  let pushers = List.assoc game_name !main_pushers in
  let game = List.find (fun game -> game.name = game_name) !games in
  let is_valid (player_name,_,_) = 
    List.exists (fun player -> player.player_name = player_name) game.players
  in
  pushers := List.filter is_valid !pushers;
  let send_downstream (_,stream,push) = 
    if not (Lwt_stream.is_closed stream) then push sendable
  in
  List.iter send_downstream !pushers

(* [send_new_player game_name player_name order] sends an update with a JSON
 * payload containing a player name and order *)
let send_new_player game_name player_name order = 
  let sendable = 
    let data = [
      "data: {";
      Printf.sprintf "data: \"playerName\": \"%s\"," player_name;
      Printf.sprintf "data: \"order\": %d" order;
      "data: }"]
    in
    let result = 
      Printf.sprintf "id: %d\r\n%s\r\n\r\n" 0 (String.concat "\r\n" data)
    in
    Some result
  in
  try send game_pushers game_name sendable with Not_found -> assert false

(* [send_diff game_name diff_string] sends an update with a JSON payload 
 * containing a diff represented as a string *)
let send_diff game_name diff_string = 
  let sendable = 
    let data = [Printf.sprintf "data: %s" diff_string] in
    let result = 
      Printf.sprintf "id: %d\r\n%s\r\n\r\n" 0 (String.concat "\r\n" data)
    in
    Some result
  in
  try send game_pushers game_name sendable with Not_found -> assert false

(* [get_info req] gets the player and game names from the request [req] *)
let get_info req = 
  let json = Yojson.Basic.from_string req.req_body in
  let player_name = json |> member "playerName" |> to_string in
  let game_name = json |> member "gameName" |> to_string in
  (player_name,game_name)

exception Exists

(* [create_game req] creates a game given the request [req] *)
let create_game req = 
  let (player_name,game_name) = get_info req in
  try
    if List.exists (fun game -> game.name = game_name) !games then raise Exists;
    let new_game = Game.create_game player_name game_name in
    games := new_game::!games;
    msg_pushers := (game_name,ref [])::!msg_pushers;
    game_pushers := (game_name,ref [])::!game_pushers;
    let res_body = Game.state_to_json new_game in
    {headers;status=`OK;res_body}
  with
  | Exists -> {
      headers=default_headers;
      status=`Bad_request;
      res_body="Game with name '" ^ game_name ^ "' already exists"
    }
  | _ -> {
      headers=default_headers;
      status=`Internal_server_error;
      res_body=server_error_msg
    }

(* [Full] is an exception that represents a full game *)
exception Full

(* [join_game req] joins a player to a game given the request [req] *)
let join_game req = 
  let (player_name,game_name) = get_info req in
  try
    let game = List.find (fun game -> game.name = game_name) !games in
    if List.exists (fun player -> player.player_name = player_name) game.players
    then raise Exists;
    let order = Game.add_player game player_name in
    send_new_player game.name player_name order;
    let res_body = Game.state_to_json game in
    {headers;status=`OK;res_body}
  with
  | Not_found -> {
      headers=default_headers;
      status=`Not_found;
      res_body="Game with name '" ^ game_name ^ "' not found"
    }
  | Full -> {
      headers=default_headers;
      status=`Bad_request;
      res_body="Game with name '" ^ game_name ^ "' is full"
    }
  | Exists -> {
      headers=default_headers;
      status=`Not_acceptable;
      res_body="Player with name '" ^ player_name ^ "' already exists in " ^
               "game with name '" ^ game_name ^ "'"
    }
  | _ -> {
      headers=default_headers;
      status=`Internal_server_error;
      res_body=server_error_msg
    }

(* [leave_game req] removes a player from a game given the request [req] *)
let leave_game req = 
  try
    let (player_name,game_name) = get_info req in
    let game = List.find (fun game -> game.name = game_name) !games in
    let (player_name,order) = Game.remove_player game player_name in
    send_new_player game_name player_name order;
    {headers;status=`OK;res_body=""}
  with
  | _ -> {
      headers=default_headers;
      status=`Internal_server_error;
      res_body=server_error_msg
    }

(* [execute_move req] performs a move on a game given the request [req] *)
let execute_move req = 
  try 
    let json = Yojson.Basic.from_string req.req_body in
    let game_name = json |> member "gameName" |> to_string in
    let game = List.find (fun game -> game.name = game_name) !games in
    let move = json |> member "move" |> Game.move_from_json in
    let diff_string = Game.execute game move |> Game.diff_to_json in
    send_diff game_name diff_string;
    {headers;status=`OK;res_body=""}
  with
  | FailedMove -> {
      headers=default_headers;
      status=`Bad_request;
      res_body="Invalid move"
    }
  | _ -> {
      headers=default_headers;
      status=`Internal_server_error;
      res_body=server_error_msg
    }

(* [subscribe main_pushers req] registers a client to recieve game updates
 * via the [main_pushers] (i.e. game updates or messages) *)
let subscribe main_pushers req = 
  try
    let headers = 
      Header.init_with "Access-Control-Allow-Origin" origin 
      |> fun header -> Header.add header "Access-Control-Allow-Headers" 
                                         "content-type"
      |> fun header -> Header.add header "Access-Control-Allow-Methods" "GET"
      |> fun header -> Header.add header "content-type" "text/event-stream"
      |> fun header -> Header.add header "cache-control" "no-cache"
    in
    let game_name = List.assoc "gameName" req.params in
    let player_name = List.assoc "playerName" req.params in
    let (st,push_st) = Lwt_stream.create () in
    let body = Cohttp_lwt_body.of_stream st in
    let pushers = List.assoc game_name !main_pushers in
    pushers := (player_name,st,push_st)::!pushers;
    Server.respond ~headers ~flush:true ~status:`OK ~body ()
  with 
  | Not_found -> Server.respond ~headers:default_headers ~status:`Not_found 
                                ~body:(Cohttp_lwt_body.of_string "") ()
  | _ -> Server.respond ~headers:default_headers ~status:`Internal_server_error
                        ~body:(Cohttp_lwt_body.of_string server_error_msg) ()

(* [subscribe_updates req] registers a client to receive game updates *)
let subscribe_updates = 
  subscribe game_pushers

(* [subscribe_messaging req] registers a client to receive messages *)
let subscribe_messaging = 
  subscribe msg_pushers

(* [send_message req] sends a message from a player to a game *)
let send_message req = 
  let create_msg player_name msg = 
    let escaped_msg = String.escaped msg in
    let data = [
      "data: {";
      Printf.sprintf "data: \"msg\": \"%s\"," escaped_msg;
      Printf.sprintf "data: \"playerName\": \"%s\"" player_name;
      "data: }"]
    in
    let result = 
      Printf.sprintf "id: %d\r\n%s\r\n\r\n" 0 (String.concat "\r\n" data)
    in
    Some result
  in
  let json = Yojson.Basic.from_string req.req_body in
  let (player_name,game_name) = get_info req in
  let msg = json |> member "msg" |> to_string in
  try
    send msg_pushers game_name (create_msg player_name msg);
    {headers;status=`OK;res_body=""}
  with
  | Not_found -> {
      headers=default_headers;
      status=`Not_found;
      res_body="Game with name '" ^ game_name ^ "' not found or player with " ^ 
               "name '" ^ player_name ^ "' not found in game"
    }
  | _ -> {
      headers=default_headers;
      status=`Internal_server_error;
      res_body=server_error_msg
    }

let _ = 
  Game.init_names ();
  HttpServer.add_route (`OPTIONS,"/api/game") cors_control;
  HttpServer.add_custom_route (`GET,"/api/game") subscribe_updates;
  HttpServer.add_route (`PUT,"/api/game") create_game;
  HttpServer.add_route (`POST,"/api/game") join_game;
  HttpServer.add_route (`DELETE,"/api/game") leave_game;
  HttpServer.add_route (`OPTIONS,"/api/move") cors_control;
  HttpServer.add_route (`POST,"/api/move") execute_move;
  HttpServer.add_route (`OPTIONS,"/api/messaging") cors_control;
  HttpServer.add_custom_route (`GET,"/api/messaging") subscribe_messaging;
  HttpServer.add_route (`POST,"/api/messaging") send_message;
  HttpServer.run ~port:8000 ()
