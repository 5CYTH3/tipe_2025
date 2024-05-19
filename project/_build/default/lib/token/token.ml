type literal = 
    | Str of string
    | Int of int
    | Bool of bool
;;

type t =
    | LParen
    | RParen
    | Id of string
    | Literal of literal
;;

