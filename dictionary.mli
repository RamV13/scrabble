
(*A [node] is composed of a character and a map from characters to children nodes*)
type node

(*Basic required trie functionality*)
val empty : node
val is_empty : node -> bool
val find : string -> node -> node
val add : string -> node -> node
val remove: string -> node -> node
val mem: string -> node -> bool

(*[is_leaf] returns whether or not a given string is fully extended*)
val is_leaf : string -> bool

(*[extenstions] returns all possible extentions of an input string*)
val extensions : string -> string list

(*[make] is a function which accepts a string and returns a completed trie*)
val make : string -> node