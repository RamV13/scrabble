
open Dom

open Lwt

(* [fail] is a failure callback *)
let fail = fun _ -> assert false

(* [get_element_by_id id] gets a DOM element by [id] *)
let get_element_by_id id = 
  Js.Opt.get Dom_html.document##getElementById (Js.string id) fail

let onload _ =
  Js._false

let _ = 
  Dom_html.window##onload <- Dom_html.handler onload
