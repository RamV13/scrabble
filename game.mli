
type player = {
  player_name : string;
  tiles : char list;
  score : int;
  order : int;
  ai : bool
}

type state = {
  id : int;
  name : string;
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
