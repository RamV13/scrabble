
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

let get_game_info _ = 
  {
    headers = Header.init ();
    meth = `GET;
    url = "http://127.0.0.1:8000/api/games";
    req_body = ""
  }
  |> XHRClient.exec
  >>= fun res -> 
      begin
        (
        match res.status with
        | `OK ->
          begin
            let info_of_json json = 
              (json |> member "id" |> to_int,json |> member "name" |> to_string)
            in
            from_string res.res_body
            |> to_list
            |> List.map info_of_json
          end
        | _ -> Dom_html.window##alert (Js.string "Failed Request"); []
        )
        |> Lwt.return
      end

let empty_state =
  {
    id = 0;
    name = "";
    grid = Grid.empty;
    players = [];
    remaining_tiles = [];
    turn = 0
  }

let join_game id name = 
  Lwt.return empty_state (* TODO *)

let create_game name = 
  Lwt.return empty_state (* TODO *)

let get_game_state id = 
  Lwt.return empty_state (* TODO *)

let execute_move id move = 
  Lwt.return empty_state (* TODO *)
