
open Cohttp

(* [request] contains HTTP request headers, method (i.e. GET/POST/etc.), url, 
 * and string body *)
type request = { 
  headers : Header.t;
  meth : Code.meth;
  url : string;
  req_body : string
}

(* [reponse] contains HTTP response headers, status code, and a string body *)
type response = {
  headers : Header.t;
  status : Code.status_code;
  res_body : string
}

(* [callback] is a function that processes the contents of a response *)
type callback = response -> unit

(* [exec request] performs a synchronous HTTP request with the [request] payload
 * and provides the value of the response data *)
val exec : request -> response

(* [exec request callback] performs an HTTP request with the [request] payload 
 * and asynchronously sends response data to the [callback] provided *)
val exec_async : request -> callback -> unit
