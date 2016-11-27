# Assignment 3: Search

Authors: Justin Parrat, Brian Shi, Kirk Thaker, Ram Vellanki <br/>
NetID's: jwp258, bcs84, kt485, rsv32

## System Description

**TODO**

**Citations**
- OCaml documentation [http://caml.inria.fr/pub/docs/manual-ocaml/libref/index.html](http://caml.inria.fr/pub/docs/manual-ocaml/libref/index.html)
- Js_of_ocaml documentation [https://ocsigen.org/js_of_ocaml/](https://ocsigen.org/js_of_ocaml/)
- Js_of_ocaml source code [https://github.com/ocsigen/js_of_ocaml/](https://github.com/ocsigen/js_of_ocaml/)
- Ocamlbuild documentation [https://github.com/ocaml/ocamlbuild](https://github.com/ocaml/ocamlbuild)
- Cohttp source code and documentation [https://github.com/mirage/ocaml-cohttp](https://github.com/mirage/ocaml-cohttp)
- Lwt source code and documentation [https://github.com/ocsigen/lwt](https://github.com/ocsigen/lwt)

## Architecture

**TODO (pull from MS1)**

## System Design

**TODO (pull from MS1)**

## Module Design

**TODO (pull from MS1)**

## Data

## External Dependencies
- ounit - for unit testing
- lwt (**2.6.0**) - for concurrent programming
- cohttp - for the HTTP client and server
- yojson - for serializing/deserializing data to and from JSON
- js_of_ocaml - for developing a browser-compatible GUI

\* use `opam update` and `opam upgrade` to fix unbound value compile errors

## Testing

### Test Plan

**TODO (pull from MS1)**

### Test Results

**TODO**

### Known Problems

- only guaranteed to work on Chrome
- may experience issues with the board view if the screen is too small (simple fix is zooming out on the page)
- no error handling/fault tolerance for loss of connection (e.g. WiFi dropping out) due to the nature of the `EventSource` API but the server will not crash as a result of any of these issues
- no real security guarantees (outside of browser CORS security and Cornell's hosting security guarantees) although we do recognize certain vulnerabilities that would be simple to protect against given more time 
  - incorporating a user and game id mechanism and employing hashing and/or authentication to prevent against unwanted requests to modify game state
  - concealing other player's tiles in server-sent payloads involving game state
- open multiple instances of the game on one machine within one browser (i.e. in different tabs) will **NOT** work due to the limitations of 6 `EventSource` instances per browser (may work up to 2-3 tabs at most)

## Division of Labor

**TODO**

# TODO SHOULD THE CONTENT BELOW BE INCLUDED

### Code Design

Types are sometimes specified to assist OCaml in overcoming the pitfalls of weak type inference with records or refs and also to improve code clarity in some cases.

#### UI
- used 'id' for each 'td' tag because of limitations of `Js_of_ocaml` in searching through an HTML table
- used viewport pixels to scale well with multiple screen sizes
- newly joined users may not view older chats only newly received chats (design decision of messaging system)
- attempts to load the `scrabble.html` page without joining/creating a game will lead to an automatic redirection to the `index.html` (main) page

#### Server-Client

##### OCaml
- used association lists (as opposed to a `Map` or `Hashtbl`) as the primary data structure for storing key-value pairings despite the O(n) lookup time because all lists on the server
are consistently < 10,000 in length (efficiency traded off in favor of simplicity)
- `refs` were employed extensively in order to save state on the server side because immutable abstractions are impossible when each individual endpoint callback is process independently in its own scope and own thread (alternative would be to use a database such as `sqlite3` for persistent storage but again efficiency was traded off in favor of simplicity)

##### Web Security
- web application is only accessible on Cornell's network for security reasons
- browser CORS security protects against malicious rewritten JavaScript requests
- due to Cornell's hosting only opening port 80, all requests to the uri "/api" on port 80 are forwarded to an internal OCaml server running on port 8000

##### Web
- used SSE (Server-Sent Events) as opposed to polling techniques (for fast downstream game updates and messages)  
  \* chose not to use WebSockets because of the minimal support for OCaml
- used `localStorage` to transfer data between the two pages of the web application
- used 200 HTTP status code for all successful operations for simplicity and ease of development given time constraints (as opposed to employing other more descriptive status code such as 201)
- `HttpServer` and `HttpClient` modules provide clean interfaces that abstract what would've been duplicated code between HTTP endpoints/requests

`HttpServer` Usage:  
```ocaml
HttpServer.add_route (<HTTP_METHOD>,<ROUTE_ENDPOINT>) <CALLBACK>
e.g. HttpServer.add_route (`GET,"/api/") callback
```

**TODO**

### Programming

Each individual module was implemented **bottom-up** while the overall project was implemented **top-down** in order to allow for parallel development of modules while stubbing out dependencies.

**TODO**
