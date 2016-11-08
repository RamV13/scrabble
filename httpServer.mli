
open Cohttp

(* [meth] is a type synonym for HTTP request methods (i.e. GET/POST/etc.) *)
type meth = Code.meth

(* [request] contains HTTP headers, query parameters, and a string body *)
type request = { 
  headers : Header.t;
  params : (string * string) list;
  req_body : string
}

(* [response] contains HTTP headers, status code, and a string body *)
type response = {
  headers : Header.t;
  status : Code.status_code;
  res_body : string
}

(* [callback] is a function to convert a request into a simple HTTP response *)
type callback = request -> response

(* [custom_callback] is a function to convert a request into a cohttp response 
 * which enables further customization of the response mechanism *)
type custom_callback = request -> (Response.t * Cohttp_lwt_body.t) Lwt.t

(* [add_route (meth,route) callback] adds a route to the server where the 
 * [route] is tied to a callback [callback] and is served with the HTTP method 
 * [meth] on the uri [route] *)
val add_route : meth * string -> callback -> unit

(* [add_custom_route (meth,route) callback] adds a route to the server where the
 * [route] is tied to the [callback] and is served with the HTTP method [meth]
 * on the uri [route] *)
val add_custom_route : Code.meth * string -> custom_callback -> unit

(* [run server] runs a server that listens on an optional port parameter which 
 * defaults to 8000 *)
val run : ?port:int -> unit -> unit
