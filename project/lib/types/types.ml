type t =
    | Bool
    | Str
    | Int
    | List of t * t
    | Nil
    [@@deriving show];;
