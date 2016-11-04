
open Dom

let onload _ =
  let body = Js.Opt.get (Dom_html.document##getElementById (Js.string "body")) (fun () -> assert false) in
  let createButton ?show:(show=Js._false) ?value:(value=None) title action = 
    let but = Dom_html.createInput ?_type:(Some (Js.string "submit")) Dom_html.document in
    but##value <- Js.string title;
    but##className <- Js.string "mdl-button mdl-js-button mdl-button--raised mdl-button--colored mdl-js-ripple-effect";
    Dom.appendChild body but;
  in
  (* ignore (createButton "Button" "Button"); *)
  Js._false

let _ = 
  Dom_html.window##onload <- Dom_html.handler onload
