type player = {
  player_id : string;
  game_id : string;
  tiles : char list;
  score : int;
  order : int;
  ai : bool
}

type state = {
  id : string;
  grid: Grid.board;
  players : player list;
  remaining_tiles : char list;
  turn: int
}

type move = {
  tiles_placed : (char * (int * int)) list;
  player: string
}

val move : state -> move -> state 

