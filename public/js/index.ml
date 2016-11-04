
open Dom

(* [fail] is a failure callback *)
let fail = fun () -> assert false

(* [get_element_by_id id] gets a DOM element by [id] *)
let get_element_by_id id = 
  Js.Opt.get Dom_html.document##getElementById (Js.string id) fail

let toggle_join = ref true
let toggle_create = ref true

(* [handle_btn_join btn ()] is the callback to handle the click events of the 
 * join button [btn] *)
let handle_btn_join btn _ = 
  if !toggle_join then 
    btn##style##cssText <- Js.string "background: #FF9800; width: 100px; margin-right: 10px"
  else 
    btn##style##cssText <- Js.string "background: #009688; width: 100px; margin-right: 10px";
  toggle_join := not !toggle_join;
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
  Js._false

let _ = 
  Dom_html.window##onload <- Dom_html.handler onload
