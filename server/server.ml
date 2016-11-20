
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

(* [names_file] is the file containing a list of line separated names *)
let names_file = "names.txt"
(* [names] is the list of computer names *)
let names = ref []

(* [origin] is the allowed origin to request this server *)
let origin = "*"

(* initialize names *)
let _ = 
  let input_channel = open_in names_file in
  try
    let rec process_line () = 
      let line = input_line input_channel in
      names := line::!names;
      process_line ()
    in
    ignore (process_line ());
    close_in input_channel
  with
  | End_of_file -> close_in input_channel
  | exc -> close_in_noerr input_channel; raise exc

(* [default_headers] are the set of default headers for plain text responses *)
let default_headers =
  Header.init_with "content-type" "text/plain"
  |> fun header -> Header.add header "Access-Control-Allow-Origin" origin

(* [headers] are the default set of headers for JSON responses *)
let headers = 
  Header.init_with "content-type" "application/json"
  |> fun header -> Header.add header "Access-Control-Allow-Origin" origin

(* [cors_control req] responds with Access-Control headers to enable CORS *)
let cors_control req = 
  let headers = 
    Header.init_with "Access-Control-Allow-Origin" origin 
    |> fun header -> Header.add header "Access-Control-Allow-Headers" 
                                       "content-type"
    |> fun header -> Header.add header "Access-Control-Allow-Methods"
                                       "GET,POST,PUT,OPTIONS"
  in
  {headers;status=`OK;res_body=""}

(* [send_new_score game_name player_name order score] sends an update with a 
 * JSON payload containg a player name, associated score, and order *)
let send_new_score game_name player_name order score = 
  let sendable = 
    let data = [
      "data: {";
      Printf.sprintf "data: \"playerName\": \"%s\"," player_name;
      Printf.sprintf "data: \"order\": %d," order;
      Printf.sprintf "data: \"score\": %d" score;
      "data: }"]
    in
    let result = 
      Printf.sprintf "id: %d\r\n%s\r\n\r\n" 0 (String.concat "\r\n" data)
    in
    Some result
  in
  let send_update (stream,push) = 
    if not (Lwt_stream.is_closed stream) then push sendable
  in
  try
    let pushers = !(List.assoc game_name !game_pushers) in
    List.iter (fun pusher -> send_update pusher) pushers
  with Not_found -> assert false

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
    let grid = Grid.empty in
    let base_player = {player_name="";tiles=[];score=0;order=0;ai=true} in
    let create_player order = 
      (* TODO populate tiles *)
      let player_name = (List.hd !names) ^ " (AI)" in
      names := List.tl !names;
      {base_player with player_name;order}
    in
    let players = 
      let rec add_players acc order = 
        if order < 4 then add_players ((create_player order)::acc) (order + 1)
        else acc
      in
      add_players [{base_player with player_name;ai=false}] 1
      |> List.rev
    in
    (* TODO populate remaining tiles *)
    let remaining_tiles = [] in
    let new_game = {name=game_name;grid;players;remaining_tiles;turn=0} in
    games := new_game::!games;
    msg_pushers := (game_name,ref [])::!msg_pushers;
    game_pushers := (game_name,ref [])::!game_pushers;
    let res_body = Game.to_json new_game in
    {headers;status=`OK;res_body}
  with
  | Exists -> {
      headers=default_headers;
      status=`Bad_request;
      res_body="Game with name " ^ game_name ^ " already exists"
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
    let substituted = 
      try List.find (fun player -> player.ai) game.players
      with Not_found -> raise Full
    in
    substituted.player_name <- player_name;
    substituted.ai <- false;
    send_new_score game.name player_name substituted.order substituted.score;
    let res_body = Game.to_json game in
    {headers;status=`OK;res_body}
  with
  | Not_found -> {
      headers=default_headers;
      status=`Not_found;
      res_body="Game with name " ^ game_name ^ " not found"
    }
  | Full -> {
      headers=default_headers;
      status=`Bad_request;
      res_body="Game with name " ^ game_name ^ " is full"
    }
  | Exists -> {
      headers=default_headers;
      status=`Not_acceptable;
      res_body="Player with name " ^ player_name ^ " already exists in game " ^
                game_name
    }

(* [leave_game req] removes a player from a game given the request [req] *)
let leave_game req = 
  let json = Yojson.Basic.from_string req.req_body in
  let game_name = json |> member "gameName" |> to_string in
  let player_name = json |> member "playerName" |> to_string in
  {headers;status=`OK;res_body=""}

(* [subscribe main_pushers req] registers a client to recieve game updates
 * via the [main_pushers] (i.e. game updates or messages) *)
let subscribe main_pushers req = 
  try
    let headers = 
      Header.init_with "Access-Control-Allow-Origin" "*" 
      |> fun header -> Header.add header "Access-Control-Allow-Headers" 
                                         "content-type"
      |> fun header -> Header.add header "Access-Control-Allow-Methods" "GET"
      |> fun header -> Header.add header "content-type" "text/event-stream"
      |> fun header -> Header.add header "cache-control" "no-cache"
    in
    let game_name = List.assoc "gameName" req.params in
    let (st,push_st) = Lwt_stream.create () in
    let body = Cohttp_lwt_body.of_stream st in
    let pushers = List.assoc game_name !main_pushers in
    Lwt_stream.closed st >>= (fun () -> print_endline "closed" |> return) |> Lwt_main.run;
    pushers := (st,push_st)::!pushers;
    Server.respond ~headers ~flush:true ~status:`OK ~body ()
  with 
  | Not_found -> Server.respond ~headers:default_headers ~status:`Not_found 
                                ~body:(Cohttp_lwt_body.of_string "") ()

(* [subscribe_updates req] registers a client to receive game updates *)
let subscribe_updates = 
  subscribe game_pushers

(* [subscribe_messaging req] registers a client to receive messages *)
let subscribe_messaging = 
  subscribe msg_pushers

(* [send_message req] sends a message from a player to a game *)
let send_message req = 
  let create_msg player_name msg = 
    let data = [
      "data: {";
      Printf.sprintf "data: \"msg\": \"%s\"," msg;
      Printf.sprintf "data: \"playerName\": \"%s\"" player_name;
      "data: }"]
    in
    let result = 
      Printf.sprintf "id: %d\r\n%s\r\n\r\n" 0 (String.concat "\r\n" data)
    in
    Some result
  in
  let send_msg (stream,push) player_name msg = 
    if not (Lwt_stream.is_closed stream) then push (create_msg player_name msg)
  in
  let json = Yojson.Basic.from_string req.req_body in
  let (player_name,game_name) = get_info req in
  let msg = json |> member "msg" |> to_string in
  try
    let pushers = !(List.assoc game_name !msg_pushers) in
    List.iter (fun pusher -> send_msg pusher player_name msg) pushers;
    {headers;status=`OK;res_body=""}
  with
  | Not_found -> {
      headers=default_headers;
      status=`Not_found;
      res_body="Game with name " ^ game_name ^ " not found or player with name "
                ^ player_name ^ " not found in game"
    }

let _ = 
  HttpServer.add_route (`OPTIONS,"/api/game") cors_control;
  HttpServer.add_custom_route (`GET,"/api/game") subscribe_updates;
  HttpServer.add_route (`PUT,"/api/game") create_game;
  HttpServer.add_route (`POST,"/api/game") join_game;
  HttpServer.add_route (`DELETE,"/api/game") leave_game;
  HttpServer.add_route (`OPTIONS,"/api/messaging") cors_control;
  HttpServer.add_custom_route (`GET,"/api/messaging") subscribe_messaging;
  HttpServer.add_route (`POST,"/api/messaging") send_message;
  HttpServer.run ~port:8000 ()
