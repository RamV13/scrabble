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
  [('e',1);('a',1);('i',1);('o',1);('n',1);('r',1);('t',1);('l',1);('s',1);('u',1);
   ('d',2);('g',2);
   ('b',3);('c',3);('m',3);('p',3);
   ('f',4);('h',4);('v',4);('w',4);('y',4);
   ('k',5);
   ('j',8);('x',8);
   ('q',10);('z',10);
   ('?',0)]

let create_bag () = 
  [ 'a';'a';'a';'a';'a';'a';'a';'a';'a';
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

(* get the prefix of a specific cell in a given direction *)
let get_prefix board ((row,col),tile) dir = 
  let rec get_prev (x,y) (acc_w,acc_s) dx dy = 
    let prev_neighbor = 
      if dir = Horizontal then (Grid.get_neighbors board x y).left 
      else (Grid.get_neighbors board x y).top
    in
    match prev_neighbor with
    | Some c ->
      let tile = Char.escaped c in
      let tile_val = List.assoc c tile_values in
      get_prev (x + dx, y + dy) (tile ^ acc_w, acc_s + tile_val) dx dy
    | None -> (acc_w,acc_s)
  in
  let (dx,dy) = if dir = Horizontal then (-1,0) else (0,-1) in
  get_prev (row,col) ("",0) dx dy

(* get the suffix of a specific cell in a given direction *)
let get_suffix board ((row,col),tile) dir = 
  let rec get_next (x,y) (acc_w,acc_s) dx dy = 
    let next_neighbor = 
      if dir = Horizontal then (Grid.get_neighbors board x y).right 
      else (Grid.get_neighbors board x y).bottom
    in
    match next_neighbor with
    | Some c ->
      let tile = Char.escaped c in
      let tile_val = List.assoc c tile_values in
      get_next (x + dx, y + dy) (acc_w ^ tile, acc_s + tile_val) dx dy
    | None -> (acc_w,acc_s)
  in
  let (dx,dy) = if dir = Horizontal then (1,0) else (0,1) in
  get_next (row,col) ("",0) dx dy

let rec get_words board tp dir = 
  let valid_skips lst dir (x0,y0) = 
    let z0 = if dir = Horizontal then x0 else y0 in
    let (_,break) = List.fold_left 
          (fun (acc,found) ((x,y),_) -> 
            let z = if dir = Horizontal then x else y in
            if z = acc + 2 then (z,found @ [acc + 1])
            else if z > acc + 2 then raise (FailedMove "skip too large")
            else (z,found)) 
          (z0 - 1,[]) lst
    in
    break
  in
  match dir with
  | Horizontal -> 
    let t = List.sort (fun ((x1,_),_) ((x2,_),_) -> Pervasives.compare x1 x2) tp in
    let ((x0,y0),_) = List.hd t in
    get_words_horiz board t (valid_skips t Horizontal (x0,y0)) (x0,y0)
  | Vertical -> 
    let t = List.sort (fun ((_,y1),_) ((_,y2),_) -> Pervasives.compare y1 y2) tp in
    let ((x0,y0),_) = List.hd t in
    get_words_vert board t (valid_skips t Vertical (x0,y0)) (x0,y0)

and get_words_horiz b tp breaks (x0,y0)= 
  let words = List.fold_left 
    (fun acc ((x,y),c) -> 
      let (prefix,p_sc) = get_prefix b ((x,y),c) Vertical in
      let (suffix,s_sc) = get_suffix b ((x,y),c) Vertical in
      let tile_mult = Grid.bonus_letter_at (x,y) in
      let word_mult = Grid.bonus_word_at (x,y) in
      let tile_val = try List.assoc c tile_values with _ -> raise (FailedMove "u suck") in
      let new_w = (prefix ^ (Char.escaped c) ^ suffix, (p_sc + s_sc + tile_mult * tile_val)*word_mult) in
      if String.length (fst new_w) > 1 then new_w::acc else acc)
    [] tp
  in
  let (prefix,p_sc) = get_horiz_prefix b (try List.hd tp with _ -> raise (FailedMove "u suck1")) in
  let (suffix,s_sc) = get_horiz_suffix b (try List.hd (List.rev tp) with _ -> raise (FailedMove "u suck2")) in
  let count = ref x0 in
  let (infix,i_sc) = List.fold_left 
    (fun (acc_w,acc_s) ((x,y),c) -> 
      if x = !count then 
        begin
          count := !count + 1;
          let tile_mult = Grid.bonus_letter_at (x,y) in
          (acc_w ^ (Char.escaped c),tile_mult * (try List.assoc c tile_values with _ -> raise (FailedMove "u suck3")) + acc_s)
        end
      else 
        begin
          count := !count + 2; 
          let tile = 
            match Grid.get_tile b (x - 1) y with
            | Some character -> character 
            | None -> raise (FailedMove "gap in tiles placed")
          in
          let tile_mult = Grid.bonus_letter_at (x,y) in
          (acc_w ^ (Char.escaped tile) ^ (Char.escaped c), tile_mult * (try List.assoc c tile_values with _ -> raise (FailedMove "u suck4")) + acc_s)
        end
    ) ("",0) tp 
  in
  print_endline (prefix ^ infix ^ suffix);
  let word_mult = 
    List.map (fun x -> Grid.bonus_word_at (x,y0)) breaks
    |> List.fold_left (fun acc x -> acc*x) 1
  in
  let h_word = (prefix ^ infix ^ suffix,(p_sc + i_sc + s_sc)*word_mult) in
  print_endline (string_of_int (snd h_word));
  h_word::words

and get_words_vert b tp breaks (x0,y0) = 
  let words = List.fold_left 
    (fun acc ((x,y),c) -> 
      let (prefix,p_sc) = get_prefix b ((x,y),c) Horizontal in
      let (suffix,s_sc) = get_suffix b ((x,y),c) Horizontal in
      let tile_mult = Grid.bonus_letter_at (x,y) in
      let word_mult = Grid.bonus_word_at (x,y) in
      let tile_val = List.assoc c tile_values in
      let new_w = (prefix ^ (Char.escaped c) ^ suffix, (p_sc + s_sc + tile_mult * tile_val)*word_mult) in
      if String.length (fst new_w) > 1 then new_w::acc else acc)
    [] tp
  in
  let (prefix,p_sc) = get_prefix b (List.hd tp) Vertical in
  let (suffix,s_sc) = get_suffix b (List.hd (List.rev tp)) Vertical in
  let count = ref y0 in
  let (infix,i_sc) = List.fold_left 
    (fun (acc_w,acc_s) ((x,y),c) -> 
      if y = !count then 
        begin
          count := !count + 1;
          let tile_mult = Grid.bonus_letter_at (x,y) in
          (acc_w ^ (Char.escaped c),tile_mult * (List.assoc c tile_values) + acc_s)
        end
      else 
        begin
          count := !count + 2; 
          let tile = 
            match Grid.get_tile b x (y - 1) with
            | Some character -> character 
            | None -> raise (FailedMove "gap in tiles placed")
          in
          let tile_mult = Grid.bonus_letter_at (x,y) in
          (acc_w ^ (Char.escaped tile) ^ (Char.escaped c), tile_mult * (List.assoc c tile_values) + acc_s)
        end
    ) ("",0) tp 
  in
  let word_mult = 
    List.map (fun y -> Grid.bonus_word_at (x0,y)) breaks
    |> List.fold_left (fun acc x -> acc*x) 1
  in
  let v_word = (prefix ^ infix ^ suffix,(p_sc + i_sc + s_sc)*word_mult) in
  v_word::words
  
(* get the direction a word was placed in *)
let get_word_dir tp = 
  let ((x0,y0),c0) = try (List.hd tp) with _ -> assert false in
  let vert = List.fold_left (fun acc ((x,y),c) -> acc && x=x0) true tp in
  let horiz = List.fold_left (fun acc ((x,y),c) -> acc && y=y0) true tp in
  match vert,horiz with
  | false, false -> 
    raise (FailedMove "tiles must be placed horizontally or vertically")
  | true, _ -> print_endline "vert"; Vertical
  | false, true -> print_endline "horiz"; Horizontal

(*let rec calc_score board tp dir = 
  match dir with
  | Horizontal -> 
    let t = 
      List.sort (fun ((x1,_),_) ((x2,_),_) -> Pervasives.compare x1 x2) tp in
    calc_score_horiz board t
  | Vertical -> 
    let t = 
      List.sort (fun ((_,y1),_) ((_,y2),_) -> Pervasives.compare y1 y2) tp in
    calc_score_vert board t

and calc_score_horiz board tp = 
  let h_word_total = List.fold_left 
    (fun acc ((x,y),c) -> 
      let ((_,_),tile_mult) = 
        try List.find (fun ((r,c),m) -> r = x && c = y) Grid.bonus_letter_tiles 
        with _ -> ((0,0),1) (* the 0s are irrelevant *)
      in
      acc + (List.assoc c tile_values)*tile_mult
    ) 
    0 tp
  in
  let h_word_bonuses = List.filter (fun ((r,c),_) -> List.mem_assoc (r,c) tp) Grid.bonus_word_tiles in
  let h_word_score = List.fold_left (fun acc ((_,_),m) -> acc * m) h_word_total h_word_bonuses in
  h_word_score (* based on assumption that tiles are consecutive *)

and calc_score_vert board tp = 
  0*)

(* [execute state move] executes a [move] to produce a new game state from the 
 * previous game state [state] *)
let execute s move =
  print_endline "here";
  let m = 
    let new_placed_tiles = 
      List.map (fun ((a,b),c) -> ((b,a),Char.lowercase_ascii c)) move.tiles_placed
    in
    {move with tiles_placed=new_placed_tiles}
  in
  let tiles_pl = m.tiles_placed in
  let p_n = m.player in
  let cur_p = 
    try List.find (fun p -> p.player_name = p_n) s.players
    with Not_found -> assert false
  in
  assert (cur_p.order = s.turn);
  print_endline "1";
  let (words,words_sc) = List.split (get_words s.grid tiles_pl (get_word_dir tiles_pl)) in
  if List.fold_left (fun acc w -> acc && Dictionary.in_dict w) true words then
    begin
    (* place tiles *)
    List.iter (fun ((x,y),c) -> s.grid <- (Grid.place s.grid x y c);) tiles_pl;
    (* todo BINGO *)
    let calc_score = List.fold_left (fun acc x -> acc + x) 0 words_sc in
    cur_p.score <- (cur_p.score + calc_score);
    s.turn <- ((s.turn + 1) mod 4);
    {board_diff = tiles_pl; new_turn_val = s.turn; players_diff = [cur_p]}
    end
  else
    raise (FailedMove "an illegimate word was formed")

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
