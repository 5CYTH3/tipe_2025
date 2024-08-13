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

let rec parse_lambdas (program: Token.t list) (env: Types.env): traversal =
    let (arg, rest) = parse_arg program in
    let tv = Types.fresh_tv () in
    let env' = Types.extend_env env arg tv in (* Maps the new type variable to the arg in a new context *)
    let { expr = body; t; subst; rest = rest'; } = parse_lambda_body rest env' in 
    {
        expr = Abs (arg, body);
        t = Types.Abs (Types.apply_subst subst tv, t);
        subst;
        rest = rest';
    }

and parse_arg program: string * Token.t list =
    match program with
    | Token.Id i :: t -> (i, t)
    | _ -> failwith "Unexpected token, expected an identifier"

and parse_lambda_body (program: Token.t list) (env: Types.env): traversal =
    match program with
    | Dot :: t -> parse_expr t env
    | _ -> failwith "Expected lambda body definition (starting with a dot '.')"

and parse_app (f: expr) (program: Token.t list) (env: Types.env): traversal =
    match program with
    (* Need to make sure that the first two cases receive the right environment *)
    | Id x :: t -> parse_app (App (f, Var x)) t env
    | Literal x :: t -> parse_app (App (f, match_literals x)) t env
    | _ -> 
        let { e } = f in

and parse_expr (program: Token.t list) (env: Types.env): traversal =
    match program with
    | Id i :: t -> parse_app (Var i) t 
    | Lambda :: t -> parse_lambdas t env
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

