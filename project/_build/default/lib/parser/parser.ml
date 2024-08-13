open Token;;

type literal =
    | Str of string
    | Int of int
    | Bool of bool
[@@deriving show]

type expr =
    | Var of string
    | Abs of string * expr
    | App of expr * expr
    | Let of string * expr * expr
    | Literal of literal
[@@deriving show]

type ast = expr list
[@@deriving show]

type traversal = {
    expr: expr;
    t: Types.t;
    subst: Types.subst;
    rest: Token.t list;
}

let match_literals = function
    | Token.Str s -> Literal (Str s)
    | Token.Int i -> Literal (Int i)
    | Token.Bool b -> Literal(Bool b)
;;

let type_literals = function
    | Token.Str _ ->  Types.Str
    | Token.Int _ -> Types.Int
    | Token.Bool _ -> Types.Bool
;;

let rec parse_lambdas (program: Token.t list): traversal =
    let (arg, rest) = parse_arg program in
    let { expr = body; t; subst; rest = rest'; } = parse_lambda_body rest in 
    let tv = Types.
    {
        expr = Abs (arg, body)
        t = ;
        subst = ;
        rest = rest';
    }

and parse_arg program: string * Token.t list =
    match program with
    | Token.Id i :: t -> (i, t)
    | _ -> failwith "Unexpected token, expected an identifier"

and parse_lambda_body program: traversal =
    match program with
    | Dot :: t -> parse_expr t
    | _ -> failwith "Expected lambda body definition (starting with a dot '.')"

and parse_app f program: traversal =
    match program with
    | Id x :: t -> parse_app (App (f, Var x)) t
    | Literal x :: t -> parse_app (App (f, match_literals x)) t
    | _ -> (f, program)

and parse_expr program: traversal =
    match program with
    | Id i :: t -> parse_app (Var i) t 
    | Lambda :: t -> parse_lambdas t
    | Token.Literal l :: t -> 
        {
            expr = match_literals l;
            t = type_literals l;
            subst = Types.TypeMap.empty;
            rest = t;
        }  
    (* Forgot the `in` clause. Need to parse it. *)
    | Let :: Id i :: Assign :: t -> 
        let { expr = body; t; subst; rest; } = parse_expr t in

        {
            expr = Abs (i, body);
            t = ;
            subst = ;
            rest;
        }
        
    | _ -> failwith "Token not recognized"
;;

let rec parse (program: Token.t list) =
    let (expr, rest) = parse_expr program in
    match rest with
    | [] -> [expr]
    | l -> expr :: (parse l)
;;

