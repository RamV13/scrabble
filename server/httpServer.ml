
open Lwt
open Cohttp
open Cohttp_lwt_unix

type meth = Code.meth
type uri = Uri.t

type request = { 
                 headers : Header.t;
                 params : (string * string) list;
                 req_body : string
               }

type response = {
                  headers : Header.t;
                  status : Code.status_code;
                  res_body : string
                }

type callback = request -> response

type custom_callback = request -> (Response.t * Cohttp_lwt_body.t) Lwt.t

type route = (meth * uri) * callback

type t = route list

let server = ref []

let add_route (meth,uri) callback = 
  let server_callback req = 
    let res = callback req in
    Server.respond ~headers:res.headers ~status:res.status
                   ~body:(Cohttp_lwt_body.of_string res.res_body) ()
  in
  server := ((meth,uri),server_callback)::!server

let add_custom_route (meth,uri) custom_callback = 
  server := ((meth,uri),custom_callback)::!server

let callback _ req body = 
  let meth = Request.meth req in
  let uri = req |> Request.uri in
  try
    let headers = Request.headers req in
    let params = 
      List.map (fun query -> ((fst query),List.hd (snd query))) (Uri.query uri)
    in
    body
    |> Cohttp_lwt_body.to_string
    >>= fun req_body -> 
        begin
          {headers;params;req_body} 
          |> List.assoc (meth,Uri.path uri) !server
        end
  with Not_found -> Server.respond_string ~status:`Not_found ~body:"" ()

let run ?(port=8000) _ = 
  Lwt.async_exception_hook := ignore;
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
  |> Lwt_main.run
  |> ignore
