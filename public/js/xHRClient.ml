
open Lwt
open Cohttp
module Client = Cohttp_lwt_xhr.Client
module Client_sync = Cohttp_lwt_xhr.Client_sync

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

(* [exec req] performs the HTTP request [req] asynchronously by using the 
 * monadic operators of Lwt *)
let exec (req : request) = 
  Client.call ~headers:req.headers
              ~body:(Cohttp_lwt_body.of_string req.req_body) 
              req.meth (Uri.of_string req.url)
  >>= fun (res,body) -> body |> Cohttp_lwt_body.to_string
  >|= fun body -> {
    headers = Response.headers res;
    status = Response.status res;
    res_body = body
  }

(* [exec_sync req] performs the HTTP request [req] synchronously by using a
 * synchronous version of the Cohttp XHR client *)
let exec_sync (req : request) = 
  Client_sync.call ~headers:req.headers
                   ~body:(Cohttp_lwt_body.of_string req.req_body) 
                    req.meth (Uri.of_string req.url)
  >>= fun (res,body) -> body |> Cohttp_lwt_body.to_string
  >|= fun body -> {
    headers = Response.headers res;
    status = Response.status res;
    res_body = body
  }
