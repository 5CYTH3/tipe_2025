open Token;;

type literal =
    | Str of string
    | Id of string
    | Int of int
    | Bool of bool
[@@deriving show]

type expr =
    | List of expr list
    | Atom of literal
[@@deriving show]


type ast = expr list
[@@deriving show]

(* Parsing expressions beginning with the character `(` *)
let rec parse_lists (program: Token.t list): expr * Token.t list =
    match program with
    | LParen :: t -> begin
        (* Enters the parsing list "state" *)
        let (expr, rest) = parse_list_content t in 
        match rest with
        (* Deleting the tailing parenthese *)
        | RParen :: t -> (expr, t)
        | _ -> failwith "Missing closing parenthesis"
    end
    | _ -> failwith "Unexpected Token, expected LParen."
and parse_list_content (program: Token.t list): expr * Token.t list =
    match program with
    (* Handling the case of an empty list `()` *)
    | RParen :: t -> (List [], RParen :: t)
    | _ -> begin
        let (atom, rest) = parse_atoms program in
        let (expr, rest') = parse_list_content rest in
        match expr with
        | List l -> (List (atom :: l), rest')
        | Atom _ -> failwith "This is not supposed to happen"
    end
and parse_atoms (program: Token.t list): expr * Token.t list = 
    match program with
    | LParen :: _ -> parse_lists program
    | Id i :: t -> (Atom (Id i), t)
    | Literal l :: t -> begin
        match l with
        | Int i -> (Atom (Int i), t)
        | Str s -> (Atom (Str s), t)
        | Bool b -> (Atom (Bool b), t)
    end
    | _ -> failwith "Orphan closing parenthese `RPAREN`"
;;

let rec parse (program: Token.t list): ast =
    let (expr, rest) = parse_atoms program in
    match rest with
    | [] -> [expr]
    | l -> expr :: (parse l)
;;

