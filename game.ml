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

type direction = Vertical | Horizontal

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

let tile_values = 
  [('E',1);('A',1);('I',1);('O',1);('N',1);('R',1);('T',1);('L',1);('S',1);('U',1);
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

(* convert a string of length 1 to character *)
let str_to_c s = 
  if String.length s <> 1 then failwith "str_to_char"
  else String.get s 0

(* given bag and n # of tiles to take, return tiles taken and new bag. helper
 * function for create_game *)
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

(* creates a new game given a new player name [p_n] and game name [g_n]. will
 * randomly assign AI names and distribute tiles to everyone *)
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

(* execute =================================================================*)

(* get the prefix and its score of a specific cell in a given direction *)
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

(* get the suffix and its score of a specific cell in a given direction *)
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

let get_words_dir b tp (y0,x0) dir = 
  let opp dir = 
    if dir = Horizontal then Vertical else Horizontal
  in
  let words = List.fold_left 
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
  (*if dir = Horizontal then print_endline "horiz" else print_endline "vert";*)
  let (prefix,p_sc) = 
    get_prefix b (try List.hd tp with _ -> assert false) dir in
  let (suffix,s_sc) = 
    get_suffix b (try List.hd (List.rev tp) with _ -> assert false) dir in
  let z0 = if dir = Horizontal then x0 else y0 in
  let count = ref z0 in
  let (infix,i_sc) = List.fold_left 
    (fun (acc_w,acc_s) ((y,x),c) -> 
      let z = if dir = Horizontal then x else y in
      if z = !count then 
        begin
          count := !count + 1;
          let tile_mult = Grid.bonus_letter_at (y,x) in
          (acc_w ^ Char.escaped c,tile_mult*(List.assoc c tile_values) + acc_s)
        end
      else 
        begin
          count := !count + 2; 
          let tile = 
            let (dx,dy) = if dir = Horizontal then (-1,0) else (0,-1) in
            match Grid.get_tile b (y + dy) (x + dx) with
            | Some character -> character 
            | None -> raise (FailedMove "gap in tiles placed")
          in
          let tile_mult = Grid.bonus_letter_at (y,x) in
          (acc_w ^ (Char.escaped tile) ^ (Char.escaped c), tile_mult * (List.assoc c tile_values) + acc_s)
        end
    ) ("",0) tp 
  in
  print_endline ("prefix: "^prefix ^ " infix: " ^ infix ^ " suffix: " ^ suffix);
  let word_mult = 
    List.map (fun ((y,x),_) -> Grid.bonus_word_at (y,x)) tp 
    |> List.fold_left (fun acc x -> acc*x) 1
  in
  print_endline ("main word multiplier: " ^ string_of_int word_mult);
  let word = (prefix ^ infix ^ suffix,(p_sc + i_sc + s_sc)*word_mult) in
  let final_words = word::words in
  if final_words = [] && prefix = "" && suffix = "" && not (b = Grid.empty) then 
    raise (FailedMove "cannot place tiles apart from existing ones")
  else
   final_words

(* get all new words and their scores formed when tiles [tp] are placed in 
 * direction [dir].
 * the actual work is done in the two functions below *)
let get_words board tp dir = 
  let valid_skips lst dir (y0,x0) = 
    let z0 = if dir = Horizontal then x0 else y0 in
    let (_,v) = List.fold_left 
      (fun (acc,valid) ((y,x),_) -> 
        let z = if dir = Horizontal then x else y in
        if z = acc + 2 then (z,valid)
        else if z > acc + 2 then (z,false)
        else (z,valid)) 
      (z0 - 1,true) lst
    in
    v
  in
  match dir with
  | Horizontal -> 
    let t = List.sort (fun ((_,x1),_) ((_,x2),_) -> Pervasives.compare x1 x2) tp in
    let ((y0,x0),_) = try List.hd t with _ -> assert false in
    if valid_skips t Horizontal (y0,x0) then get_words_dir board t (y0,x0) Horizontal
    else raise (FailedMove "skip too large")
  | Vertical -> 
    let t = List.sort (fun ((y1,_),_) ((y2,_),_) -> Pervasives.compare y1 y2) tp in
    let ((y0,x0),_) = try List.hd t with _ -> assert false in
    if valid_skips t Vertical (y0,x0) then get_words_dir board t (y0,x0) Vertical
    else raise (FailedMove "skip too large")
  
(* get the direction a word was placed in *)
let get_word_dir b tp = 
  let ((y0,x0),c0) = try (List.hd tp) with _ -> assert false in
  let get_dir_single_char () = 
    let t = List.hd tp in
    let vert_prefix = fst (get_prefix b t Vertical) in
    let vert_suffix = fst (get_suffix b t Vertical) in
    let horiz_prefix = fst (get_prefix b t Horizontal) in
    let horiz_suffix = fst (get_suffix b t Horizontal) in
    if vert_prefix = "" && vert_suffix = "" && (horiz_prefix <> "" || horiz_suffix <> "") then Horizontal
    else if horiz_prefix = "" && horiz_suffix = "" && (vert_prefix <> "" || vert_suffix <> "") then Vertical
    else if vert_prefix = "" && vert_suffix = "" && horiz_prefix = "" && horiz_suffix = "" then raise (FailedMove "cannot place single tile by itself")
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
    | true, true -> failwith "TODO: single tile infer direction"
  in
  if List.length tp = 1 then get_dir_single_char () else get_dir_multiple_chars ()

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
      if List.mem h r then aux (remove_from_list h r []) t (* cur tile played is in rack *)
      else if List.mem '?' r then aux (remove_from_list '?' r []) t (* cur tile played not in rack, but can use blank tile *)
      else assert false (* there's a tile played which player never had *)
    | [] -> r
  in
  aux rack played

(* [execute state move] executes a [move] to produce a new game state from the 
 * previous game state [state] *)
(* ram is assuming that players_diff is always list of length 1 *)
let execute s move =
  let tiles_pl = move.tiles_placed in

  let p_n = move.player in
  let cur_p = 
    try List.find (fun p -> p.player_name = p_n) s.players
    with Not_found -> assert false
  in
  assert (cur_p.order = s.turn);
  let (words,words_sc) = 
    List.split (get_words s.grid tiles_pl (get_word_dir s.grid tiles_pl)) in
  let words_cap = List.map (fun w -> String.lowercase_ascii w) words in
  print_string "words: "; List.iter (fun x -> print_string (x ^ ", ")) words; print_endline "";
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
    assert (List.length new_tiles = 7);
    cur_p.score <- (cur_p.score + calc_score);
    cur_p.tiles <- new_tiles;
    s.turn <- ((s.turn + 1) mod 4);
    s.remaining_tiles <- new_bag;
    {board_diff = tiles_pl; new_turn_val = s.turn; players_diff = [cur_p]}
    end
  else
    begin
      let bad_words = List.fold_left (fun acc x -> acc ^ (if acc <> "" then ", " else "") ^ x) "" invalid_words in
      raise (FailedMove ("illegimate word(s) formed: " ^ bad_words))
    end

(* ===========================================================================
 * JSON methods below *)

(* converts a character list to its json representation. helper function for 
 * state_to_json *)
let char_list_to_json lst =
  let rec aux l acc =
    match l with
    | h::[] -> acc ^ "\"" ^ (Char.escaped h) ^ "\""
    | h::t -> aux t (acc ^ "\"" ^ (Char.escaped h) ^ "\",")
    | [] -> acc
  in
  "[" ^ aux lst "" ^ "]"

(* converts a list of players to its json representation. players are 
 * represented with json objects. helper function for state_to_json and 
 * diff_to_json *)
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
  (string_of_int state.turn) ^ "}"

(* converts json list of players to list of players. helper function for 
 * state_from_json and diff_from_json *)
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

(* converts a board_diff or tiles_placed to json object. It can be used for both
 * because both are list of ((int,int),char). helper function for diff_to_json
 * and move_to_json *)
let board_diff_to_json board_diff = 
  let rec aux bd acc = 
    match bd with 
    | ((row,col),value)::[] -> 
      acc ^ "{\"row\":" ^ (string_of_int row) ^ ",\"col\":" ^ (string_of_int col) ^ 
      ",\"value\":\"" ^ (Char.escaped value) ^ "\"}"
    | ((row,col),value)::t -> 
      (acc ^ "{\"row\":" ^ (string_of_int row) ^ ",\"col\":" ^ (string_of_int col) ^ 
      ",\"value\":\"" ^ (Char.escaped value) ^ "\"},")
      |> aux t
    | [] -> acc
  in
  aux board_diff ""

(* converts json representation of tiles placed to list of positions and chars.
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

(* convert diff to its json representation *)
let diff_to_json d = 
  "{\"board_diff\": [" ^ (board_diff_to_json d.board_diff) ^ 
  "],\"new_turn_val\": " ^ (string_of_int d.new_turn_val) ^ 
  ",\"players_diff\": [" ^ (players_to_json d.players_diff) ^ "]}"

let diff_from_json json = 
  let b = member "board_diff" json |> to_list |> json_tp_to_tp in
  let t = member "new_turn_val" json |> to_int in
  let p = member "players_diff" json |> to_list |> json_players_to_players in
  {board_diff = b; new_turn_val = t; players_diff = p}

(* converts a move to its json representation *)
let move_to_json m = 
  "{\"tilesPlaced\": [" ^ (board_diff_to_json m.tiles_placed) ^ 
  "], \"playerName\": \"" ^ m.player ^ "\"}"

(* converts json to its move representation *)
let move_from_json json = 
  let p = member "playerName" json |> to_string in
  let tp = member "tilesPlaced" json |> to_list |> json_tp_to_tp in
  {player = p; tiles_placed = tp}

(* let _ = 
  let print_board b = 
    let print_row r = 
      List.iter (fun x -> 
        match x with 
        | Some t -> print_string ((Char.escaped t) ^ " ");
        | None -> print_string "# ";
      ) r
    in
    List.iter (fun row -> print_row row; print_endline "";) b;
  in
  (*print_endline ("hello: " ^ (string_of_bool (Dictionary.in_dict "hello")));
  print_endline ("hen: " ^ (string_of_bool (Dictionary.in_dict "hen")));
  print_endline ("ten: " ^ (string_of_bool (Dictionary.in_dict "ten")));*)
  init_names();
  let s = create_game "Brian" "game" in
  print_endline ("turn: " ^ (string_of_int s.turn));
  print_board s.grid;  
  (*let m_ten = {
    tiles_placed = [((5,5),'h');((6,5),'e');((7,5),'n')];
    player = "Brian"
  } in
  let _ = execute s m_ten in
  print_board s.grid;
  let player2 = List.nth (s.players) 1 in
  let m_hen = {
    tiles_placed = [((6,4),'t');((6,6),'n')];
    player = player2.player_name
  } in
  let _ = execute s m_hen in*)
  let m1 = {
    tiles_placed = [((5,5),'z');((6,5),'e');((7,5),'n');((8,5),'i');((9,5),'t');((10,5),'h')];
    player = "Brian"
  } in
  let _ = execute s m1 in
  print_board s.grid;
  (*let player2 = List.nth (s.players) 1 in
  let m2 = {
    tiles_placed = [((8,5),'l');((9,5),'s')];
    player = player2.player_name
  } in
  let _ = execute s m2 in
  print_endline ("turn: " ^ (string_of_int s.turn));
  print_board s.grid;*)
*)
