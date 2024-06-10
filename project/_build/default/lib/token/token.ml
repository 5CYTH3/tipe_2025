type literal = 
    | Str of string
    | Int of int
    | Bool of bool
    [@@deriving show];;

type t =
    | LParen
    | RParen
    | Id of string
    | Literal of literal
    [@@deriving show];;

