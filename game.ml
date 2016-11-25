open Yojson.Basic.Util

exception Full
exception FailedMove

(* [player] contains the player's identification information, tiles, score,
 * order in the game, and a flag indicating whether this player is an AI *)
type player = {
  mutable player_name : string;
  mutable tiles : char list;
  mutable score : int;
  order : int;
  mutable ai : bool
}

(* [state] contains the game's identification information, board, players,
 * remaining tiles (i.e. bag), and turn *)
type state = {
  name : string;
  mutable grid: Grid.board;
  players : player list;
  mutable remaining_tiles : char list;
  mutable turn: int
}

(* [move] is a representation of a game move containing an association list of
 * characters to be placed in specific coordinates as well as the player id of
 * the player who performs the move *)
type move = {
  tiles_placed : ((int * int) * char) list;
  player : string
}

(* [diff] is a representation of the difference between two game states. There
 * is no field for the difference in the grids because it will always either
 * be no difference (in the case of adding/removing players or a failed move) or
 * the move given to a state *)
type diff = {
  board_diff : ((int * int) * char) list;
  new_turn_val : int;
  players_diff : player list
}

(* for initializing names to be used in create game *)
(* [names_file] is the file containing a list of line separated names *)
let names_file = "names.txt"
(* [names] is the list of computer names *)
let names = ref []

let init_names () = 
  let input_channel = open_in names_file in
  try
    let rec process_line () = 
      let line = input_line input_channel in
      names := line::!names;
      process_line ()
    in
    ignore (process_line ());
    close_in input_channel
  with
  | End_of_file -> close_in input_channel
  | exc -> close_in_noerr input_channel; raise exc

let create_bag () = 
  [
    'a';'a';'a';'a';'a';'a';'a';'a';'a';
    'b';'b';
    'c';'c';
    'd';'d';'d';'d';
    'e';'e';'e';'e';'e';'e';'e';'e';'e';'e';'e';'e';
    'f';'f';
    'g';'g';'g';
    'h';'h';
    'i';'i';'i';'i';'i';'i';'i';'i';'i';
    'j';
    'k';'k';
    'l';'l';'l';'l';
    'm';'m';
    'n';'n';'n';'n';'n';'n';
    'o';'o';'o';'o';'o';'o';'o';'o';
    'p';'p';
    'q';
    'r';'r';'r';'r';'r';'r';
    's';'s';'s';'s';
    't';'t';'t';'t';'t';'t';
    'u';'u';'u';'u';
    'v';'v';
    'w';'w';
    'x';
    'y';'y';
    'z';
    '?';'?'
  ]

(* convert a string of length 1 to character *)
let str_to_c s = 
  if String.length s <> 1 then failwith "str_to_char"
  else String.get s 0

(* [add_player state player_id player_name] adds the player with name 
 * [player_name] to the current game [state], and returns the turn order that
 * the player was added to (i.e. the turn of the AI who the player replaced).
 * The player replaces the computer with the lowest order, and inherits its 
 * tiles, score, and turn. 
 * raise Failure if the game is full of players (non computer) already *)
let add_player s p_n = 
  let substituted = 
      try List.find (fun player -> player.ai) s.players
      with Not_found -> raise Full
  in
  substituted.player_name <- p_n;
  substituted.ai <- false;
  substituted.order

(* [remove_player state player_id] removes the player with id [player_id] from
 * current game [state], and returns the new state. It replaces the old player
 * with a computer that inherits the removed player's tiles, score, turn, and id
 * raises Failure if there is no player in the game with [player_id] *)
let remove_player (s : state) (p_n : string) : (string * int) = 
  let substituted = 
    try List.find (fun player -> player.player_name = p_n) s.players
    with Not_found -> assert false
  in
  substituted.player_name <- ((List.hd !names) ^ " (AI)");
  names := List.tl !names;
  substituted.ai <- true;
  (substituted.player_name,substituted.order)

(* [execute state move] executes a [move] to produce a new game state from the 
 * previous game state [state] *)
let execute s m =
  let tiles_pl = m.tiles_placed in
  let p_n = m.player in
  let substituted = 
    try List.find (fun player -> player.player_name = p_n) s.players
    with Not_found -> assert false
  in
  substituted.score <- (substituted.score + 1);
  s.turn <- ((s.turn + 1) mod 4);
  (* later: player diff sometimes is just empty *)
  {board_diff = tiles_pl; new_turn_val = s.turn; players_diff = [substituted]}

(* =========================================================================
 * following are all for to_json *)
let char_list_to_json lst =
  let rec aux l acc =
    match l with
    | h::[] -> acc ^ "\"" ^ (Char.escaped h) ^ "\""
    | h::t -> aux t (acc ^ "\"" ^ (Char.escaped h) ^ "\",")
    | [] -> acc
  in
  "[" ^ aux lst "" ^ "]"

let players_to_json players =
  let rec aux p acc =
    match p with
    | h::[] -> 
      acc ^ "{\"player_name\":\"" ^ h.player_name ^ "\",\"tiles\":" ^ 
      (char_list_to_json h.tiles) ^ ",\"score\": " ^ (string_of_int h.score) ^ 
      ",\"order\": " ^ (string_of_int h.order) ^ ",\"ai\": " ^ 
      (string_of_bool h.ai) ^"}"
    | h::t -> 
      (acc ^ "{\"player_name\":\"" ^ h.player_name ^ "\",\"tiles\":" ^ 
      (char_list_to_json h.tiles) ^ ",\"score\": " ^ (string_of_int h.score) ^ 
      ",\"order\": " ^ (string_of_int h.order) ^ ",\"ai\": " ^ 
      (string_of_bool h.ai) ^"},") 
      |> aux t
    | [] -> acc
  in
  aux players ""

(* [to_json state] is a json representation of [state] *)
let to_json state = 
  "{\"name\": \"" ^ state.name ^ "\",\"grid\":" ^ (Grid.to_json state.grid) ^ 
  ",\"players\":[" ^ (players_to_json state.players) ^ "],\"remaining_tiles\": " 
  ^ (char_list_to_json state.remaining_tiles) ^ ",\"turn\": " ^ 
  (string_of_int state.turn) ^ "}"

(* to_json done
 * ========================================================================== 
 * the following are for from_json *)

let json_players_to_players json_l =
  let rec aux j_l acc = 
    match j_l with
    | h::t -> 
      let new_p = 
        {
          player_name = member "player_name" h |> to_string;
          tiles = List.map 
            (fun x -> x |> to_string |> str_to_c) (member "tiles" h |> to_list);
          score = member "score" h |> to_int;
          order = member "order" h |> to_int;
          ai = member "ai" h |> to_bool;
        } 
      in
      aux t (new_p :: acc)
    | [] -> acc
  in
  aux json_l []

(* [from_json Yojson.Basic.json] is a [state] converted from its json 
 * representation *)
let from_json json = 
  let n = member "name" json |> to_string in
  let g = member "grid" json |> Grid.from_json in
  let p = member "players" json |> to_list |> json_players_to_players in
  let r = 
    member "remaining_tiles" json 
    |> to_list 
    |> List.map (fun x -> x |> to_string |> str_to_c) 
  in
  let t = member "turn" json |> to_int in
  {
    name = n;
    grid = g;
    players = p;
    remaining_tiles = r;
    turn = t
  }

(* from_json done
 * ===========================================================
 * following are for create_game *)

(* given bag and n # of tiles to take, return tiles taken and new bag *)
let take_tiles bag num_to_take = 
  let take_tile bag index = 
    let count = ref 0 in
    let (tile, new_bag) = List.fold_left 
      (fun (t,b) c -> 
        if !count = index && t = None then (Some c,b)
        else (count := !count + 1; (t, c::b)))
      (None,[]) bag
    in
    match tile with 
    | None -> assert false
    | Some t -> (t,List.rev new_bag)
  in
  Random.self_init ();
  let rec aux tiles b n =
    if n = 0 then (tiles,b)
    else 
      let (t,bag') = take_tile b (Random.int (List.length b)) in 
      aux (t::tiles) bag' (n - 1)
  in
  aux [] bag num_to_take

let create_game p_n g_n = 
  let grid = Grid.empty in
  let bag = create_bag () in
  let base_player = {player_name=""; tiles=[]; score=0; order=0; ai=true} in
  let create_ai order bag =
    let player_name = (List.hd !names) ^ " (AI)" in
    names := List.tl !names;
    let (tiles,bag') = take_tiles bag 7 in 
    ({base_player with player_name; order; tiles}, bag')
  in
  let (players,new_bag) = 
    let rec add_players acc order b = 
      if order < 4 then 
        let (new_player,b') = create_ai order b in
        add_players (new_player::acc) (order + 1) b'
      else (acc,b)
    in
    let (human_tiles,bag') = take_tiles bag 7 in
    add_players 
      [{base_player with player_name = p_n; ai=false; tiles = human_tiles}] 
      1 bag'
  in
  {name=g_n; grid; players=List.rev players; remaining_tiles=new_bag; turn=0}

(* create_game done
 * ========================================================================
 * following is for diff_to_json *)

let board_diff_to_json board_diff = 
  let rec aux bd acc = 
    match bd with 
    | ((row,col),value)::[] -> 
      acc ^ "{\"row\":" ^ (string_of_int row) ^ ",\"col\":" ^ (string_of_int col) ^ 
      ",\"value\":\"" ^ (Char.escaped value) ^ "\"}"
    | ((row,col),value)::t -> 
      (acc ^ "{\"row\":" ^ (string_of_int row) ^ ",\"y\":" ^ (string_of_int col) ^ 
      ",\"value\":\"" ^ (Char.escaped value) ^ "\"},")
      |> aux t
    | [] -> acc
  in
  aux board_diff ""

let diff_to_json d = 
  "{\"board_diff\": [" ^ (board_diff_to_json d.board_diff) ^ 
  "],\"new_turn_val\": " ^ (string_of_int d.new_turn_val) ^ 
  ",\"players_diff\": [" ^ (players_to_json d.players_diff) ^ "]}"

(* diff_to_json done
 * ======================================================================
 * following is for move_from_json *)

let json_tp_to_tp tiles_placed = 
  let rec aux tp acc = 
    match tp with
    | h::t -> 
      let row = member "row" h |> to_int in
      let col = member "col" h |> to_int in
      let value = member "value" h |> to_string |> str_to_c in
      aux t (((row,col),value)::acc)
    | [] -> acc
  in
  aux tiles_placed []

let move_from_json json = 
  let p = member "playerName" json |> to_string in
  let tp = member "tilesPlaced" json |> to_list |> json_tp_to_tp in
  {player = p; tiles_placed = tp}

(* let _ = 
  (*init_names ();
  (* create_game "Brian" "mygame" |> to_json |> print_endline *)
  let game = create_game "Brian" "mygame" in 
  let x = add_player game "Ram" in 
  let y = remove_player game "Brian" in 
  game |> to_json |> print_endline *)
  let p = 
    {
      player_name = "Brian";
      tiles = ['a';'b';'c';'d';'e';'f';'g'];
      score = 100;
      order = 2;
      ai = false
    }
  in
  let x = 
    {
      board_diff = [((0,0),'a');((1,1),'e')];
      new_turn_val = 3;
      players_diff = [p]
    }
  in
  let rec string_of_my_list lst = 
    let rec aux l acc = 
      match l with 
      | ((x,y),c)::t -> aux t (acc ^ "((" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ ")," ^ (Char.escaped c) ^ ");")
      | [] -> acc
    in
    aux lst ""
  in
  let json = "{\"tiles_placed\": [{\"x\":0,\"y\":0,\"char\":\"a\"},{\"x\":1,\"y\":1,\"char\":\"e\"}],\"player\": \"Brian\"}" in
  (* diff_to_json x *)
  let move = move_from_json (Yojson.Basic.from_string json) in
  print_endline (move.player);
  print_endline (string_of_my_list [((0,0),'a');((1,1),'b')]);
  print_endline (move.tiles_placed |> string_of_my_list) *)



