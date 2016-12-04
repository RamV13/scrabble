open Yojson.Basic.Util
open Grid
open Dictionary

exception Full
exception FailedMove of string

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
  mutable turn: int;
  mutable score_history : int list
}

(* [move] is a representation of a game move containing an association list of
 * characters to be placed in specific coordinates as well as the player id of
 * the player who performs the move *)
type move = {
  tiles_placed : ((int * int) * char) list;
  player : string;
  swap : char list;
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

type direction = Vertical | Horizontal

(* for initializing names to be used in create game *)
(* [names_file] is the file containing a list of line separated names *)
let names_file = "names.txt"
(* [names] is the list of computer names *)
let names = ref []

(* initialize list of names from names.txt *)
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

let tile_values =
  [('E',1);('A',1);('I',1);('O',1);('N',1);('R',1);('T',1);('L',1);('S',1);
   ('U',1);
   ('D',2);('G',2);
   ('B',3);('C',3);('M',3);('P',3);
   ('F',4);('H',4);('V',4);('W',4);('Y',4);
   ('K',5);
   ('J',8);('X',8);
   ('Q',10);('Z',10);
   ('?',0)]

let create_bag () =
  [ 'A';'A';'A';'A';'A';'A';'A';'A';'A';
    'B';'B';
    'C';'C';
    'D';'D';'D';'D';
    'E';'E';'E';'E';'E';'E';'E';'E';'E';'E';'E';'E';
    'F';'F';
    'G';'G';'G';
    'H';'H';
    'I';'I';'I';'I';'I';'I';'I';'I';'I';
    'J';
    'K';'K';
    'L';'L';'L';'L';
    'M';'M';
    'N';'N';'N';'N';'N';'N';
    'O';'O';'O';'O';'O';'O';'O';'O';
    'P';'P';
    'Q';
    'R';'R';'R';'R';'R';'R';
    'S';'S';'S';'S';
    'T';'T';'T';'T';'T';'T';
    'U';'U';'U';'U';
    'V';'V';
    'W';'W';
    'X';
    'Y';'Y';
    'Z';
    '?';'?']

(* convert a string [s] of length 1 to character *)
let str_to_c s =
  if String.length s <> 1 then failwith "str_to_char"
  else String.get s 0

(* given [bag] and number of tiles to take [num_to_take], return tiles taken
 * and new bag.
 * helper function for create_game *)
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
    if n = 0 || List.length b = 0 then (tiles,b)
    else
      let (t,bag') = take_tile b (Random.int (List.length b)) in
      aux (t::tiles) bag' (n - 1)
  in
  aux [] bag num_to_take

(* creates a new game given a new player name [p_n] and game name [g_n]. will
 * randomly assign AI names and distribute tiles to everyone *)
let create_game p_n g_n =
  let grid = Grid.empty in
  let bag = create_bag () in
  let base_player = {player_name=""; tiles=[]; score=0; order=0; ai=true} in
  let create_ai order bag =
    let player_name =
      (try List.hd !names with _ -> init_names (); List.hd !names) ^ " (AI)" in
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
  {
    name = g_n;
    grid = grid;
    players = List.rev players;
    remaining_tiles = new_bag;
    turn = 0;
    score_history = [0;0;0;0;0;0]
  }

(* [add_player state player_id player_name] adds the player with name
 * [player_name] to the current game [state], and returns the turn order that
 * the player was added to (i.e. the turn of the AI who the player replaced).
 * The player replaces the computer with the lowest order, and inherits its
 * tiles, score, and turn.
 * raise Full if the game is full of players (non computer) already *)
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
 *)
let remove_player s p_n =
  let substituted =
    try List.find (fun player -> player.player_name = p_n) s.players
    with Not_found -> assert false
  in
  substituted.player_name <-
    ((try List.hd !names with _ -> init_names (); List.hd !names) ^ " (AI)");
  names := List.tl !names;
  substituted.ai <- true;
  (substituted.player_name,substituted.order)

(* execute =================================================================*)

(* returns the prefix and its score of a cell given by [((row,col),tile)]
 * returns direction [dir] *)
let get_prefix board ((row,col),tile) dir =
  let rec get_prev (y,x) (acc_w,acc_s) dx dy =
    let prev_neighbor =
      if dir = Horizontal then (Grid.get_neighbors board y x).left
      else (Grid.get_neighbors board y x).top
    in
    match prev_neighbor with
    | Some c ->
      let tile = Char.escaped c in
      let tile_val = List.assoc c tile_values in
      get_prev (y + dy, x + dx) (tile ^ acc_w, acc_s + tile_val) dx dy
    | None -> (acc_w,acc_s)
  in
  let (dx,dy) = if dir = Horizontal then (-1,0) else (0,-1) in
  get_prev (row,col) ("",0) dx dy

(* get the suffix and its score of a cell given by [((row,col),tile]
 * in direction [dir *)
let get_suffix board ((row,col),tile) dir =
  let rec get_next (y,x) (acc_w,acc_s) dx dy =
    let next_neighbor =
      if dir = Horizontal then (Grid.get_neighbors board y x).right
      else (Grid.get_neighbors board y x).bottom
    in
    match next_neighbor with
    | Some c ->
      let tile = Char.escaped c in
      let tile_val = List.assoc c tile_values in
      get_next (y + dy, x + dx) (acc_w ^ tile, acc_s + tile_val) dx dy
    | None -> (acc_w,acc_s)
  in
  let (dx,dy) = if dir = Horizontal then (1,0) else (0,1) in
  get_next (row,col) ("",0) dx dy

(* returns all the words and their scores formed by placing tiles [tp] starting
 * from [(y0,x0)] on a board [b] in direction [dir]
 * called by get_words *)
let get_words_dir b tp (y0,x0) dir =
  (* return the direction opposite to [dir] *)
  let opp dir =
    if dir = Horizontal then Vertical else Horizontal
  in

  (* paired list of words and respective score by going perpendicular to the
   * tiles placed *)
  let words_perp = List.fold_left
    (fun acc ((y,x),c) ->
      let (prefix,p_sc) = get_prefix b ((y,x),c) (opp dir) in
      let (suffix,s_sc) = get_suffix b ((y,x),c) (opp dir) in
      let tile_mult = Grid.bonus_letter_at (y,x) in
      let word_mult = Grid.bonus_word_at (y,x) in
      let tile_val = List.assoc c tile_values in
      let new_w =
        (prefix ^ (Char.escaped c) ^ suffix,
        word_mult * (p_sc + s_sc + tile_mult * tile_val))
      in
      if String.length (fst new_w) > 1 then new_w::acc else acc)
    [] tp
  in

  (* prefix/infix and their scores of the tiles placed *)
  let (prefix,p_sc) =
    get_prefix b (try List.hd tp with _ -> assert false) dir in
  let (suffix,s_sc) =
    get_suffix b (try List.hd (List.rev tp) with _ -> assert false) dir in

  (* given [start] and [finish], fills list going from start to finish *)
  let fill_list (start,finish) =
    let rec aux acc cur fin =
      if cur = fin then acc @ [cur]
      else aux (acc @ [cur]) (cur + 1) fin
    in
    aux [] start finish
  in

  (* all coordinates that the tiles placed spans (including gaps) *)
  let ((yn,xn),_) = List.rev tp |> List.hd in
  let infix_coords =
    fill_list (if dir = Horizontal then (x0,xn) else (y0,yn))
    |> List.map (fun t -> if dir = Horizontal then (y0,t) else (t,x0))
  in

  (* infix and its score (infix is formed by tiles placed and existing tiles in
   * the gaps *)
  let (infix,i_sc) = List.fold_left
    (fun (acc_w,acc_s) (y,x) ->
      let (tile,tile_val) =
        match Grid.get_tile b y x with
        | Some c -> (c, List.assoc c tile_values)
        | None ->
          let tile_in_tp =
            try List.assoc (y,x) tp
            with _ -> raise (FailedMove "gap in tiles placed")
          in
          let tile_mult = Grid.bonus_letter_at (y,x) in
          (tile_in_tp, tile_mult * (List.assoc tile_in_tp tile_values))
      in
      (acc_w ^ (Char.escaped tile),acc_s + tile_val)
    ) ("",0) infix_coords
  in

  (* calculate word multiplier *)
  let word_mult =
    List.map (fun ((y,x),_) -> Grid.bonus_word_at (y,x)) tp
    |> List.fold_left (fun acc x -> acc*x) 1
  in

  if words_perp = [] && prefix = "" && suffix = "" &&
  (List.length tp = List.length infix_coords) && not (b = Grid.empty) then
    raise (FailedMove "cannot place tiles apart from existing ones")
  else
   let word = (prefix ^ infix ^ suffix,(p_sc + i_sc + s_sc)*word_mult) in
   word::words_perp

(* return all new words and their scores formed when tiles [tp] are placed in
 * direction [dir] on [board] *)
let get_words board tp dir =
  match dir with
  | Horizontal ->
    let t =
      List.sort (fun ((_,x1),_) ((_,x2),_) -> Pervasives.compare x1 x2) tp in
    let ((y0,x0),_) = try List.hd t with _ -> assert false in
    get_words_dir board t (y0,x0) Horizontal
  | Vertical ->
    let t =
      List.sort (fun ((y1,_),_) ((y2,_),_) -> Pervasives.compare y1 y2) tp in
    let ((y0,x0),_) = try List.hd t with _ -> assert false in
    get_words_dir board t (y0,x0) Vertical

(* return the direction that tiles [tp] were placed on a board [b] *)
let get_word_dir b tp =
  let ((y0,x0),c0) = try (List.hd tp) with _ -> assert false in
  let get_dir_single_char () =
    let t = List.hd tp in
    let v_p = fst (get_prefix b t Vertical) in
    let v_s = fst (get_suffix b t Vertical) in
    let h_p = fst (get_prefix b t Horizontal) in
    let h_s = fst (get_suffix b t Horizontal) in
    if v_p = "" && v_s = "" && (h_p <> "" || h_s <> "") then Horizontal
    else if h_p = "" && h_s = "" && (v_p <> "" || v_s <> "") then Vertical
    else if v_p = "" && v_s = "" && h_p = "" && h_s = "" && b <> Grid.empty then
      raise (FailedMove "cannot place single tile by itself")
    else Horizontal
  in
  let get_dir_multiple_chars () =
    let vert = List.fold_left (fun acc ((y,x),c) -> acc && x=x0) true tp in
    let horiz = List.fold_left (fun acc ((y,x),c) -> acc && y=y0) true tp in
    match vert,horiz with
    | false, false ->
      raise (FailedMove "tiles must be placed horizontally or vertically")
    | true, false -> (*print_endline "VERT";*) Vertical
    | false, true -> (*print_endline "HORIZ";*) Horizontal
    | true, true -> assert false
  in
  if List.length tp = 1 then get_dir_single_char ()
  else get_dir_multiple_chars ()

(* return remaining tile rack after playing [played] from [rack] *)
(* assumes that tiles played will always be subset of rack *)
let diff_tile_rack rack played =
  let rec remove_from_list elem lst prev =
    match lst with
    | x::y::t ->
      if x = elem then prev @ (y::t)
      else if y = elem then prev @ (x::t)
      else remove_from_list elem t (prev @ [x] @ [y])
    | x::t ->
      if x = elem then prev @ t
      else remove_from_list elem t (prev @ [x])
    | [] -> failwith "not found"
  in
  let rec aux r p =
    match p with
    | h::t ->
      if List.mem h r then aux (remove_from_list h r []) t
      else if List.mem '?' r then aux (remove_from_list '?' r []) t
      else assert false (* there's a tile played which player never had *)
    | [] -> r
  in
  aux rack played

(* update score history. score history is past 6 moves, with oldest at the head*)
let update_sh old newest =
  List.tl old @ [newest]

(* return the diff created by placing tiles [tiles_pl] in state [s] by player
 * [cur_p]
 * helper function to [execute] *)
let create_diff s tiles_pl cur_p =
  let (words,words_sc) =
    List.split (get_words s.grid tiles_pl (get_word_dir s.grid tiles_pl)) in
  let words_cap = List.map (fun w -> String.lowercase_ascii w) words in
  print_string "words: ";
  List.iter (fun x -> print_string (x ^ ", ")) words;
  print_endline "\n";
  let (is_valid,invalid_words) = List.fold_left
    (fun (acc_bool,invalid_w) w ->
      if Dictionary.in_dict w then (acc_bool,invalid_w)
      else (false,w::invalid_w))
    (true,[]) words_cap
  in
  if is_valid then
    begin
    List.iter (fun ((y,x),c) -> s.grid <- (Grid.place s.grid y x c);) tiles_pl;
    let calc_score =
      List.fold_left (fun acc x -> acc + x) 0 words_sc
      + (if List.length tiles_pl = 7 then 50 else 0) (* BINGO! *)
    in
    let tiles = List.map (fun ((_,_),c) -> c) tiles_pl in
    let (tiles_taken,new_bag) =
      take_tiles s.remaining_tiles (List.length tiles) in
    let new_tiles = (diff_tile_rack cur_p.tiles tiles) @ tiles_taken in
    cur_p.score <- (cur_p.score + calc_score);
    cur_p.tiles <- new_tiles;
    s.turn <- ((s.turn + 1) mod 4);
    s.remaining_tiles <- new_bag;
    s.score_history <- update_sh s.score_history cur_p.score;
    print_endline ("SCORE HISTORY: (old to new)" ^ (List.fold_left (fun acc x -> acc ^ (string_of_int x) ^ ",") "" s.score_history));
    {board_diff = tiles_pl; new_turn_val = s.turn; players_diff = [cur_p]}
    end
  else
    begin
      let bad_words = List.fold_left
        (fun acc x -> acc ^ (if acc <> "" then ", " else "") ^ x)
        "" invalid_words
      in
      raise (FailedMove ("illegimate word(s) formed: " ^ bad_words))
    end

(* [execute state move] executes a [move] to produce a new game state from the
 * previous game state [state] *)
let execute s move =
  let tiles_pl =
    move.tiles_placed
    |> List.map (fun ((row,col),c) -> ((row,col),Char.uppercase_ascii c))
  in
  let p_n = move.player in
  let cur_p =
    try List.find (fun p -> p.player_name = p_n) s.players
    with Not_found -> assert false
  in
  if not (cur_p.order = s.turn) then raise (FailedMove "it's not your turn")
  else ();
  print_endline ("PLAYER: " ^ p_n);
  if List.length move.swap <> 0  && List.length s.remaining_tiles > 6 then
    begin
      s.turn <- ((s.turn + 1) mod 4);
      let tiles = move.swap in
      let (tiles_taken,new_bag) =
        take_tiles s.remaining_tiles (List.length tiles) in
      let new_tiles = (diff_tile_rack cur_p.tiles tiles) @ tiles_taken in
      cur_p.tiles <- new_tiles;
      s.remaining_tiles <- (new_bag @ tiles);
      print_endline "MOVE: swap (successful)";
      s.score_history <- update_sh s.score_history 0;
      print_endline ("SCORE HISTORY: (old to new)" ^ (List.fold_left (fun acc x -> acc ^ (string_of_int x) ^ ",") "" s.score_history));
      {board_diff = []; new_turn_val = s.turn; players_diff = [cur_p]}
    end
  else if List.length move.swap <> 0 && List.length s.remaining_tiles < 7 then
    begin
      print_endline "MOVE: swap (failed)";
      raise (FailedMove "less than 7 tiles remain in the bag")
    end
  else if List.length tiles_pl = 0 then
    begin
      print_endline "MOVE: pass";
      s.turn <- ((s.turn + 1) mod 4);
      s.score_history <- update_sh s.score_history 0;
      print_endline ("SCORE HISTORY: (old to new)" ^ (List.fold_left (fun acc x -> acc ^ (string_of_int x) ^ ",") "" s.score_history));
      {board_diff = []; new_turn_val = s.turn; players_diff = [cur_p]}
    end
  else
    begin
      print_endline "MOVE: place tiles";
      let on_star = List.filter (fun ((y,x),_) -> y = 7 && x = 7) tiles_pl in
      if s.grid = Grid.empty && List.length on_star = 0 then
        raise (FailedMove "first move must have one tile on star")
      else
        create_diff s tiles_pl cur_p
    end

let is_over s =
  let no_tiles = List.filter (fun p -> List.length p.tiles = 0) s.players in
  (s.remaining_tiles = [] && List.length no_tiles > 0) || (s.score_history = [0;0;0;0;0;0])

(* ===========================================================================
 * JSON methods below *)

(* converts a character list [lst] to its json representation.
 * helper function for state_to_json *)
let char_list_to_json lst =
  let rec aux l acc =
    match l with
    | h::[] -> acc ^ "\"" ^ (Char.escaped h) ^ "\""
    | h::t -> aux t (acc ^ "\"" ^ (Char.escaped h) ^ "\",")
    | [] -> acc
  in
  "[" ^ aux lst "" ^ "]"

let int_list_to_json lst =
  let rec aux l acc =
    match l with
    | h::[] -> acc ^ (string_of_int h)
    | h::t -> aux t (acc ^ (string_of_int h) ^ ",")
    | [] -> acc
  in
  "[" ^ aux lst "" ^ "]"

(* converts a list of [players] to its json representation. players are
 * represented with json objects.
 * helper function for state_to_json and diff_to_json *)
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
let state_to_json state =
  "{\"name\": \"" ^ state.name ^ "\",\"grid\":" ^ (Grid.to_json state.grid) ^
  ",\"players\":[" ^ (players_to_json state.players) ^ "],\"remaining_tiles\": "
  ^ (char_list_to_json state.remaining_tiles) ^ ",\"turn\": " ^
  (string_of_int state.turn) ^ ",\"score_history\": " ^
  (int_list_to_json state.score_history) ^ "}"

(* converts json list [json_l] of players to list of players.
 * helper function for state_from_json and diff_from_json *)
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
let state_from_json json =
  let n = member "name" json |> to_string in
  let g = member "grid" json |> Grid.from_json in
  let p = member "players" json |> to_list |> json_players_to_players in
  let r =
    member "remaining_tiles" json |> to_list
    |> List.map (fun x -> x |> to_string |> str_to_c)
  in
  let s =
    member "score_history" json |> to_list
    |> List.map (fun x -> x |> to_int)
  in
  let t = member "turn" json |> to_int in
  {
    name = n;
    grid = g;
    players = p;
    remaining_tiles = r;
    turn = t;
    score_history = s
  }

(* converts a [board_diff] or tiles_placed to json object. It can be used for
 * both because both are list of ((int,int),char).
 * helper function for diff_to_json and move_to_json *)
let board_diff_to_json board_diff =
  let rec aux bd acc =
    match bd with
    | ((row,col),value)::[] ->
      acc ^ "{\"row\":" ^ (string_of_int row) ^ ",\"col\":" ^
      (string_of_int col) ^ ",\"value\":\"" ^ (Char.escaped value) ^ "\"}"
    | ((row,col),value)::t ->
      (acc ^ "{\"row\":" ^ (string_of_int row) ^ ",\"col\":" ^
      (string_of_int col) ^ ",\"value\":\"" ^ (Char.escaped value) ^ "\"},")
      |> aux t
    | [] -> acc
  in
  aux board_diff ""

(* converts json representation of [tiles_placed] to list of (y,x) and chars.
 * helper function for move_from_json and diff_from_json *)
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

(* convert diff [d] to its json representation *)
let diff_to_json d =
  "{\"board_diff\": [" ^ (board_diff_to_json d.board_diff) ^
  "],\"new_turn_val\": " ^ (string_of_int d.new_turn_val) ^
  ",\"players_diff\": [" ^ (players_to_json d.players_diff) ^ "]}"

(* [diff] converted from its json representation *)
let diff_from_json json =
  let b = member "board_diff" json |> to_list |> json_tp_to_tp in
  let t = member "new_turn_val" json |> to_int in
  let p = member "players_diff" json |> to_list |> json_players_to_players in
  {board_diff = b; new_turn_val = t; players_diff = p}

(* converts a move [m] to its json representation *)
let move_to_json m =
  "{\"tilesPlaced\": [" ^ (board_diff_to_json m.tiles_placed) ^
  "], \"playerName\": \"" ^ m.player ^ "\", \"swappedTiles\":" ^
  (char_list_to_json m.swap) ^ "}"

(* converts [json] to its move representation *)
let move_from_json json =
  let p = member "playerName" json |> to_string in
  let tp = member "tilesPlaced" json |> to_list |> json_tp_to_tp in
  let st =
    member "swappedTiles" json |> to_list
    |> List.map (fun x -> x |> to_string |> str_to_c)
  in
  {player = p; tiles_placed = tp; swap = st}
