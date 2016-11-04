
open Lwt
open Cohttp
module Client = Cohttp_lwt_xhr.Client

type request = { 
                 headers : Header.t;
                 meth : Code.meth;
                 url : string;
                 req_body : string
               }

type response = {
                  headers : Header.t;
                  status : Code.status_code;
                  res_body : string
                }

type callback = response -> unit

(* [exec req] performs the HTTP request [req] synchronously by waiting for the 
 * response using the monadic operators of Lwt *)
let exec req = 
  (match req.meth with
  | `GET -> Client.get ~headers:req.headers (Uri.of_string req.url)
  | `POST -> Client.post ~body:(Cohttp_lwt_body.of_string req.req_body) 
                         ~headers:req.headers (Uri.of_string req.url)
  | `PUT -> Client.put ~body:(Cohttp_lwt_body.of_string req.req_body) 
                       ~headers:req.headers (Uri.of_string req.url)
  | _ -> failwith "Unsupported request method")
  >>= fun (res,body) -> body |> Cohttp_lwt_body.to_string
  >|= fun body -> {
    headers = Response.headers res;
    status = Response.status res;
    res_body = body
  }

(* [exec req] performs the HTTP request [req] asynchronously by sending the 
 * response data to the callback in a separate thread *)
(*
let exec_async req callback = 
  let open Cohttp_async in
  let open Async.Std in
  let res_callback (res,body) = 
    {
      headers = Response.headers res;
      status = Response.status res;
      res_body = "" (* TODO retrieve body *)
    }
    |> callback
  in
  (match req.meth with
    | `GET -> Client.get ~headers:req.headers (Uri.of_string req.url)
    | `POST -> Client.post ~body:(Cohttp_async.Body.of_string req.req_body) 
                           ~headers:req.headers (Uri.of_string req.url)
    | `PUT -> Client.put ~body:(Cohttp_async.Body.of_string req.req_body) 
                         ~headers:req.headers (Uri.of_string req.url)
    | _ -> failwith "Unsupported request method")
  |> upon
  |> fun f -> f res_callback
*)
