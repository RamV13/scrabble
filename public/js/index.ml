
open Dom

(* [fail] is a failure callback *)
let fail = fun () -> assert false

(* [get_element_by_id id] gets a DOM element by [id] *)
let get_element_by_id id = 
  Js.Opt.get Dom_html.document##getElementById (Js.string id) fail

(* [handle_btn_join] is the callback to handle the click events of the join 
 * button *)
let handle_btn_join _ = 
  Dom_html.window##alert (Js.string "JOIN");
  Js._true

(* [handle_btn_create] is the callback to handle the click events of the create
 * button *)
let handle_btn_create _ = 
  Dom_html.window##alert (Js.string "CREATE");
  Js._true

let onload _ =
  let btn_join = get_element_by_id "btn_join" in
  let btn_create = get_element_by_id "btn_create" in
  btn_join##onclick <- Dom_html.handler handle_btn_join;
  btn_create##onclick <- Dom_html.handler handle_btn_create;
  Js._false

let _ = 
  Dom_html.window##onload <- Dom_html.handler onload
