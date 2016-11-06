
open Dom

open Lwt

(* [fail] is a failure callback *)
let fail = fun _ -> assert false

(* [get_element_by_id id] gets a DOM element by [id] *)
let get_element_by_id id = 
  Js.Opt.get Dom_html.document##getElementById (Js.string id) fail

let toggle_join = ref true
let toggle_create = ref true

let event_source_constructor = Js.Unsafe.global##_EventSource
let event_source = jsnew event_source_constructor (Js.string "http://127.0.0.1:8000")

let test _ = 
  (
  ScrabbleClient.get_game_info ()
  >>= fun games -> 
      begin
        List.fold_left (fun acc elem -> acc + fst elem) 0 games
        |> fun result -> (* TODO use result *) Lwt.return ()
      end
  )
  |> Lwt.ignore_result

(* [handle_btn_join btn ()] is the callback to handle the click events of the 
 * join button [btn] *)
let handle_btn_join btn _ = 
  if !toggle_join then 
    btn##style##cssText <- Js.string "background: #FF9800; width: 100px; margin-right: 10px"
  else 
    btn##style##cssText <- Js.string "background: #009688; width: 100px; margin-right: 10px";
  toggle_join := not !toggle_join;
  test ();
  Js._false

(* [handle_btn_create btn ()] is the callback to handle the click events of the 
 * create button [btn] *)
let handle_btn_create btn _ = 
  if !toggle_create then 
    btn##style##cssText <- Js.string "background: #FF9800; width: 100px"
  else 
    btn##style##cssText <- Js.string "background: #009688; width: 100px";
  toggle_create := not !toggle_create;
  Js._false

let onload _ =
  let btn_join = get_element_by_id "btn_join" in
  let btn_create = get_element_by_id "btn_create" in
  btn_join##onclick <- Dom_html.handler (handle_btn_join btn_join);
  btn_create##onclick <- Dom_html.handler (handle_btn_create btn_create);
  event_source##onmessage <- Js.wrap_callback (fun e ->
    (*let json = 
      e##data
      |> Js.to_string
      |> Yojson.Basic.from_string
    in*)
    Dom_html.window##alert (e##data);
    ()
  );
  event_source##onerror <- Js.wrap_callback (fun e -> 
    Dom_html.window##alert (e##data);
    event_source##close ()
  );
  Js._false

let _ = 
  Dom_html.window##onload <- Dom_html.handler onload
