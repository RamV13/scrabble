# MS 2: Scrabble

Authors: Justin Parratt, Brian Shi, Kirk Thaker, Ram Vellanki <br/>
NetID's: jwp258, bcs84, kt485, rsv32

## System Description

We developed a multiplayer (distributed) Scrabble game that can be played with either human or computer players complete with the following features.

Key features:
- AI
- Web GUI
- multiplayer (distributed)
- Chat (Instant Messaging)
- English (not OCaml) dictionary manipulation using a Trie

We created a Scrabble game which allows users to play Scrabble, but with added features such as detecting if words are valid or not. We also implemented an AI to play words that maximize score according to tile/word bonuses and point values of letters.

The scrabble dictionary was implemented by using a prefix tree because the efficiency of the data structure is especially important for the AI.

For the server-client interface, we leveraged several OCaml packages (listed in the **External Dependencies** section of this document) to persist data across multiple players and multiple instances of games as well as provide an HTTP interface for multiplayer functionality over the internet. Our server is exposed over a public IP address such that any system on Cornell's network can access and play our game via our web application.

For the user interface, users are able to view their current available letters as well as the current board and are able to perform moves.

With regards to the comment on the usefulness of a trie - we felt that a trie was necessary because the AI would be doing lookup of many words when evaluating potential moves. A trie also allows the AI to quickly determine what words can be made from an existing word on the board by adding some additional tiles to the end of the word. This is elaborated in more detail in the **Data** section of this document.

**Citations**
- OCaml documentation [http://caml.inria.fr/pub/docs/manual-ocaml/libref/index.html](http://caml.inria.fr/pub/docs/manual-ocaml/libref/index.html)
- Js_of_ocaml documentation [https://ocsigen.org/js_of_ocaml/](https://ocsigen.org/js_of_ocaml/)
- Js_of_ocaml source code [https://github.com/ocsigen/js_of_ocaml/](https://github.com/ocsigen/js_of_ocaml/)
- Ocamlbuild documentation [https://github.com/ocaml/ocamlbuild](https://github.com/ocaml/ocamlbuild)
- Cohttp source code and documentation [https://github.com/mirage/ocaml-cohttp](https://github.com/mirage/ocaml-cohttp)
- Lwt source code and documentation [https://github.com/ocsigen/lwt](https://github.com/ocsigen/lwt)
- MDL documentation [https://getmdl.io/components/](https://getmdl.io/components/)
- JavaScript documentation [https://developer.mozilla.org/en-US/docs/Web/JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript)
- *The world's fastest scrabble program* [https://www.cs.cmu.edu/afs/cs/academic/class/15451-s06/www/lectures/scrabble.pdf](https://www.cs.cmu.edu/afs/cs/academic/class/15451-s06/www/lectures/scrabble.pdf)

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
- str - for easier string processing
- node - for serving web pages and forwarding requests to the OCaml server

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
- opening multiple instances of the game on one machine within one browser (i.e. in different tabs) will **NOT** work due to the limitations of 6 `EventSource` instances per browser (may work up to 2-3 tabs at most) and may even break the individual game attempting to be joined but will leave all other games unaffected

## Division of Labor

- Justin implemented the Grid and Dictionary modules (commits aren't logged correctly because he was commiting through the virtual machine)
- Brian implemented Game module

- Kirk implemented the AI (and can also verify that Justin did quite a bit of work, git was just acting weird on Justin's VM).

# TODO SHOULD THE CONTENT BELOW BE INCLUDED

### Code Design

Types are sometimes specified to assist OCaml in overcoming the pitfalls of weak type inference with records or refs and also to improve code clarity in some cases.

#### UI
- used 'id' even for repeated tags because of limitations of `Js_of_ocaml` in searching through an HTML table
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
- prevented DOM manipulation to influence HTTP requests by disabling inspect element functionality

##### Web
- used SSE (Server-Sent Events) as opposed to polling techniques (for fast downstream game updates and messages)  
  \* chose not to use WebSockets because of the minimal support for OCaml
- used `localStorage` to transfer data between the two pages of the web application
- used 200 HTTP status code for all successful operations for simplicity and ease of development given time constraints (as opposed to employing other more descriptive status code such as 201)
- `HttpServer` and `HttpClient` modules provide clean interfaces that abstract what would've been duplicated code between HTTP endpoints/requests such that a callback function simply needs to accept a request record and produce a response record

`HttpServer` Usage:  
```ocaml
HttpServer.add_route (<HTTP_METHOD>,<ROUTE_ENDPOINT>) <CALLBACK>
e.g. HttpServer.add_route (`GET,"/api/") callback
```

#### Dictionary
- used a `Map` to represent the children of a trie node to increase simplicity and speed of word lookups
- `refs` were used while building the tries to increase ease of use of the file reading mechanism

#### Grid
- `lists` were used to represent the grid and bonus tiles for simplicity (list sizes are small so efficiency loss is not drastic)

#### Game
- used records to represent a player, game diff, move, and game state.
- state and player are mutable records because the game information is not stored in a database (e.g. SQL) but rather in memory. We would have preferred to do this immutably, but the difficulty in using SQL with OCaml made us decide to just have the game information stored in memory and use mutable records.

#### AI
- Used for loops to efficiently and simply traverse the board (which is a 2-dimensional list).
Other than that, nothing special was used (only lists, tuples, and some variant/record data types).

### Programming

Each individual module was implemented **bottom-up** while the overall project was implemented **top-down** in order to allow for parallel development of modules while stubbing out dependencies.

#### AI
Although we used the Scrabble AI paper listed in the citations as inspiration,
not all of the algorithms described in it were implemented because it was
simpler and far more clear to do otherwise.

The key insights to building the scrabble AI were as follows:
- Scrabble moves have to be adjacent to an existing word (except if it's the first move)
- The limiting factor in generating move permutations is both our tile set and the efficiency of dictionary lookups.

The scrabble AI works as follows:
1. Identifies all "slots" on the board. Slots are simply (row,col) coordinates that are adjacent to an existing word.
2. For each slot, figure out what characters from our tile list simply cannot go there because they form invalid cross words.
3. The generated association list of (row, col) paired with the valid chars that can go there is called an anchor list.
4. For each anchor in our anchor list, the AI
attempts to build a word in all 4 directions. It uses the anchor list to cut down on permutations early and uses the function Dictionary.has_extension (or Dictionary.has_back_extensions if building backwards) to further cut down on permutations.
5. The final list of possible moves is accumulated and ranked based on tile values and board bonuses found in Game.
6. The best move is selected and returned, or if no move is possible, a GameOver exception is raised.
7. It is worth noting that raising a GameOver exception does not necessarily mean the game is over. It could also be the case that our AI in particular simply has no more moves to make because it has a "bad" tile list. The functions that call Ai.best_move account for this peculiarity.

#### Dictionary
There are two dictionaries in this project, (the official scrabble dictionary, and a dictionary of all words backwards).
The trie was implemented as follows:
- Each node represents a character in a word, a Map of all characters that can come next in a valid word, and a boolean which determines whether or not the current character marks the end of a valid word

Adding a word to a trie works as follows:

- In the case that the first character of the word does not exist in the children of the root, the word is recursively added letter by letter to the trie
- In the case that the first character already exists in children of the root, the rest of the word is added to the children of that node recursively (for example if the trie contains h-e-l-p and h-e-l-l-o is added, the word e-l-l-o will be added to the 'h' node) until there is a difference between the words at which point the new character is added to the map of children nodes (continuing our example the first 'l' in hello now has children 'l' and 'p').

The boolean in each trie node exists to determine whether the current letter marks the end of a valid word, even if it is not a leaf node (for example in the word "racecar" the 'e' and 'r' characters both mark the end of a valid word)
