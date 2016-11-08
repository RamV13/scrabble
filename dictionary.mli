
(*A node is composed of a character and a map from characters to children nodes*)
type node

val empty : node
val is_empty : node -> bool
val find : string -> node -> node
val add : string -> node -> node
val remove: string -> node -> node
val mem: string -> node -> bool

(*optional but i thought might be worth it*)
val is_leaf : string -> bool
val extensions : string -> string list
val make : string -> node
