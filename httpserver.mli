
open Cohttp

(* [meth] is a type synonym for HTTP request methods (i.e. GET/POST/etc.) *)
type meth = Code.meth

(* [request] contains HTTP headers and a string body *)
type request = { 
                 headers : Header.t;
                 req_body : string
               }

(* [response] contains HTTP headers, status code, and a string body *)
type response = {
                  headers : Header.t;
                  status : Code.status_code;
                  res_body : string
                }

(* [callback] is a function that converts a request into a response *)
type callback = request -> response

(* [add_route server route callback] adds a route to the server where the 
 * [route] is tied to a callback [callback] *)
val add_route : meth * string -> callback -> unit

(* [run server] runs a server that listens on an optional port parameter which 
 * defaults to 8000 *)
val run : ?port:int -> unit -> unit
