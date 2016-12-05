
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

(* [ai_sleep_time] is the sleeping time for an AI to perform a move *)
let ai_sleep_time = 2.75

(* [looping] is the list of game names that are currently running the AI *)
let looping = ref []

(* [keep_alive_time] is the time between keep alive server sent events *)
let keep_alive_time = 15.

(* [games] is the list of games currently running *)
let games = ref []
(* [game_pushers] is an association list from games to their respective game
 * related update clients *)
let game_pushers = ref []
(* [msg_pushers] is an association list from games to their respective message
 * clients *)
let msg_pushers = ref []

(* [api_secret] is the secret string used for API key hashing *)
let api_secret = "scrabble_key"

(* Map for API keys *)
module KeyMap = Map.Make(String)

(* [keys] is a mapping from API keys to the corresponding player and game *)
let keys = ref KeyMap.empty

(* [Unauthorized] is the exception raised for unauthorized requests *)
exception Unauthorized

(* [default_headers] are the set of default headers for plain text responses *)
let default_headers =
  Header.init_with "content-type" "text/plain"

(* [headers] are the default set of headers for JSON responses *)
let headers = 
  Header.init_with "content-type" "application/json"

(* [server_error_msg] is the default message for extraneous server errors *)
let server_error_msg = 
  "Something went wrong. Please check the status of the server"

(* [bad_json_response] is the response for requests with bad JSON *)
let bad_json_response = {
    headers=default_headers;
    status=`Bad_request;
    res_body="Bad JSON"
  }

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

(* [send_game_over game_name] sends the game over signal to a game *)
let send_game_over game_name = 
  let sendable = 
    let data = [
      "data: {";
      Printf.sprintf "data: \"over\": %s" "true";
      "data: }"]
    in
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

(* [get_key req] gets the API key from the [req] payload *)
let get_key req = 
  req.req_body
  |> Yojson.Basic.from_string
  |> member "key"
  |> to_string

(* [gen_api_key (player_name,game_name)] generates an API key for a user by MD5 
 * hashing the result of player_name + game_name + current time + a secret *)
let gen_api_key (player_name,game_name) = 
  let time_string = Unix.time () |> int_of_float |> string_of_int in
  player_name ^ game_name ^ time_string ^ api_secret
  |> Digest.string
  |> Digest.to_hex

(* [authorize_key key (player_name,game_name)] checks the API key against the 
 * mapping of valid API keys to players and raises Unauthorized if invalid *)
let authorize_key key (player_name,game_name) = 
  try 
    let (player_name',game_name') = KeyMap.find key !keys in
    if player_name <> player_name' || game_name <> game_name' 
    then raise Unauthorized
  with Not_found -> raise Unauthorized

(* [Exists] is the exception raised when a game name already exists *)
exception Exists

(* [create_game req] creates a game given the request [req] *)
let create_game req = 
  try
    let (player_name,game_name) = get_info req in
    try
      if List.exists (fun game -> game.name = game_name) !games 
      then raise Exists;
      let new_game = Game.create_game player_name game_name in
      games := new_game::!games;
      msg_pushers := (game_name,ref [])::!msg_pushers;
      game_pushers := (game_name,ref [])::!game_pushers;
      let api_key = gen_api_key (player_name,game_name) in
      let res_body = 
        "{\"key\": \"" ^ api_key ^ "\", \"game\": " ^ 
        Game.state_to_json new_game ^ "}"
      in
      keys := KeyMap.add api_key (player_name,game_name) !keys;
      {headers;status=`OK;res_body}
    with
    | Exists -> {
        headers=default_headers;
        status=`Bad_request;
        res_body="Game with name '" ^ game_name ^ "' already exists"
      }
  with
  | Yojson.Basic.Util.Type_error _
  | Yojson.Json_error _ -> bad_json_response
  | _ -> {
      headers=default_headers;
      status=`Internal_server_error;
      res_body=server_error_msg
    }

(* [Full] is an exception that represents a full game *)
exception Full

(* [join_game req] joins a player to a game given the request [req] *)
let join_game req = 
  try
    let (player_name,game_name) = get_info req in
    try
      let game = List.find (fun game -> game.name = game_name) !games in
      let order = Game.add_player game player_name in
      send_new_player game.name player_name order;
      let api_key = gen_api_key (player_name,game_name) in
      let res_body = 
        "{\"key\":\"" ^ api_key ^ "\",\"game\":" ^ Game.state_to_json game ^ "}"
      in
      keys := KeyMap.add api_key (player_name,game_name) !keys;
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
    | PlayerExists -> {
        headers=default_headers;
        status=`Not_acceptable;
        res_body="Player with name '" ^ player_name ^ "' already exists in " ^
                 "game with name '" ^ game_name ^ "'"
      }
  with
  | Yojson.Basic.Util.Type_error _
  | Yojson.Json_error _ -> bad_json_response
  | _ -> {
      headers=default_headers;
      status=`Internal_server_error;
      res_body=server_error_msg
    }

(* [loop_ai game] runs the AI moves on a game until a human player is found *)
let loop_ai game = 
  if List.mem game.name !looping then Lwt.return ()
  else
    begin
      looping := game.name::!looping;
      let reset_looping () = 
        looping := List.filter (fun name -> name <> game.name) !looping
      in
      let rec run_ai input_game = 
        Lwt_unix.sleep ai_sleep_time >>= fun () ->
          let game = 
            try List.find (fun game -> game.name = input_game.name) !games
            with Not_found -> reset_looping (); raise (Failure "")
          in
          let player = 
            List.find (fun player -> player.order = game.turn) game.players
          in
          if player.ai then 
            begin
              let move = 
                try Ai.best_move game player 
                with 
                | _ -> {
                    tiles_placed=[];
                    player=player.player_name;
                    swap=[]
                  }
              in
              try
                if Game.is_over game then
                  begin
                    send_game_over game.name;
                    reset_looping ();
                    Lwt.return ()
                  end
                else
                  begin
                    Game.execute game move
                    |> Game.diff_to_json
                    |> send_diff game.name;
                    run_ai game
                  end
              with _ -> 
                begin
                  print_newline ();
                  print_endline "ERROR: EXCEPTION IN AI LOOP";
                  print_newline ();
                  reset_looping ();
                  Lwt.return ()
                end
            end
          else (reset_looping (); Lwt.return ())
      in
      run_ai game
    end

(* [leave_game req] removes a player from a game given the request [req] *)
let leave_game req = 
  try
    let (player_name,game_name) = get_info req in
    let key = get_key req in
    authorize_key key (player_name,game_name);
    try
      let game = List.find (fun game -> game.name = game_name) !games in
      let (player_name,order) = Game.remove_player game player_name in
      if List.fold_left (fun acc player -> player.ai && acc) true game.players
      then games := List.filter (fun game -> game.name <> game_name) !games
      else
        begin
          keys := KeyMap.remove key !keys;
          send_new_player game_name player_name order;
          Lwt.async (fun () -> loop_ai game)
        end;
      {headers;status=`OK;res_body=""}
    with
    | Not_found -> {
        headers=default_headers;
        status=`Not_found;
        res_body="Player with name '" ^ player_name ^ "' does not exist in " ^
                 "game with name '" ^ game_name ^ "'"
      }
  with
  | Unauthorized -> {
      headers=default_headers;
      status=`Unauthorized;
      res_body="Invalid API key"
    }
  | Yojson.Basic.Util.Type_error _
  | Yojson.Json_error _ -> bad_json_response
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
    authorize_key (get_key req) (move.player,game_name);
    let diff_string = Game.execute game move |> Game.diff_to_json in
    send_diff game_name diff_string;
    if Game.is_over game then send_game_over game_name;
    Lwt.async (fun () -> loop_ai game);
    {headers;status=`OK;res_body=""}
  with
  | FailedMove msg -> {
      headers=default_headers;
      status=`Bad_request;
      res_body=msg
    }
  | Unauthorized -> {
      headers=default_headers;
      status=`Unauthorized;
      res_body="Invalid API key"
    }
  | Yojson.Basic.Util.Type_error _
  | Yojson.Json_error _ -> bad_json_response
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
      Header.init_with "content-type" "text/event-stream"
      |> fun header -> Header.add header "cache-control" "no-cache"
    in
    let game_name = List.assoc "gameName" req.params in
    let player_name = List.assoc "playerName" req.params in
    authorize_key (List.assoc "key" req.params) (player_name,game_name);
    let (st,push_st) = Lwt_stream.create () in
    let body = Cohttp_lwt_body.of_stream st in
    let pushers = List.assoc game_name !main_pushers in
    pushers := (player_name,st,push_st)::!pushers;
    Server.respond ~headers ~flush:true ~status:`OK ~body ()
  with 
  | Not_found -> Server.respond ~headers:default_headers ~status:`Not_found 
                                ~body:(Cohttp_lwt_body.of_string "") ()
  | Unauthorized -> 
      Server.respond ~headers:default_headers ~status:`Unauthorized
                     ~body:(Cohttp_lwt_body.of_string "Invalid API key") ()
  | Yojson.Basic.Util.Type_error _
  | Yojson.Json_error _ -> 
      Server.respond ~headers:default_headers ~status:`Bad_request
                     ~body:(Cohttp_lwt_body.of_string "Bad JSON") ()
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
  try
    let json = Yojson.Basic.from_string req.req_body in
    let (player_name,game_name) = get_info req in
    authorize_key (get_key req) (player_name,game_name);
    let msg = json |> member "msg" |> to_string in
    try
      send msg_pushers game_name (create_msg player_name msg);
      {headers;status=`OK;res_body=""}
    with
    | Not_found -> {
        headers=default_headers;
        status=`Not_found;
        res_body="Game with name '" ^ game_name ^ "' not found or player " ^ 
                 "with name '" ^ player_name ^ "' not found in game"
      }
  with
  | Unauthorized -> {
      headers=default_headers;
      status=`Unauthorized;
      res_body="Invalid API key"
    }
  | Yojson.Basic.Util.Type_error _
  | Yojson.Json_error _ -> bad_json_response
  | _ -> {
      headers=default_headers;
      status=`Internal_server_error;
      res_body=server_error_msg
    }

let _ = 
  Game.init_names ();
  Lwt.async (fun () -> 
    let rec keep_alive () = 
      Lwt_unix.sleep keep_alive_time >>= fun () -> 
        begin
          let send_keep_alives game = 
            let sendable = Some (Printf.sprintf ": \n\n") in
            (try send game_pushers game.name sendable 
            with Not_found -> assert false);
            (try send msg_pushers game.name sendable 
            with Not_found -> assert false)
          in
          List.iter send_keep_alives !games;
          keep_alive ()
        end
    in
    keep_alive ()
    |> Lwt.return
  );
  HttpServer.add_custom_route (`GET,"/api/game") subscribe_updates;
  HttpServer.add_route (`PUT,"/api/game") create_game;
  HttpServer.add_route (`POST,"/api/game") join_game;
  HttpServer.add_route (`DELETE,"/api/game") leave_game;
  HttpServer.add_route (`POST,"/api/move") execute_move;
  HttpServer.add_custom_route (`GET,"/api/messaging") subscribe_messaging;
  HttpServer.add_route (`POST,"/api/messaging") send_message;
  HttpServer.run ~port:8000 ()
