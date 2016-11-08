(*A [node] is composed of a character and a map from characters to children nodes*)
type t

(*Basic required trie functionality*)
val empty : t
val is_empty : t -> bool
val find : string -> t -> t
val add : string -> t -> t
val remove: string -> t -> t
val mem: string -> t -> bool

(*[is_leaf] returns whether or not a given string is fully extended*)
val is_leaf : string -> bool

(*[extensions] returns all possible extensions of an input string*)
val extensions : string -> string list

(*[make] is a function which accepts a string and returns a completed trie*)
val make : string -> t
