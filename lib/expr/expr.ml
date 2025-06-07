type literal =
    | Str of string
    | Int of int
    | Bool of bool
[@@deriving show]

type t =
    | Var of string
    | Abs of string * t
    | App of t * t
    | Let of string * t * t
    | Literal of literal
    [@@deriving show];;
