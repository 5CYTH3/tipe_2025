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

(* TODO: Infer this function *)
and parse_app (f: expr) (program: Token.t list) (env: Types.env): traversal =
    match program with
    (* Need to make sure that the first two cases receive the right environment *)
    | Id x :: t -> parse_app (App (f, Var x)) t env
    | Literal x :: t -> parse_app (App (f, match_literals x)) t env
    | _ -> (f, program)


and parse_ins (program: Token.t list) (env: Types.env): traversal =
    match program with
    | In :: t -> parse_expr t env
    | _ -> failwith "No 'in' clause given. "
and parse_expr (program: Token.t list) (env: Types.env): traversal =
    match program with
    | Id i :: t -> parse_app (Var i) t env
    | Lambda :: t -> parse_lambdas t env
    | Token.Literal l :: t -> 
        {
            expr = match_literals l;
            t = type_literals l;
            subst = Types.TypeMap.empty;
            rest = t;
        }  

    | Let :: Id i :: Assign :: t -> 
        let { expr = body; t = t1; subst = subst1; rest; } = parse_expr t env in
        let env' = Types.TypeMap.map (fun v -> Types.apply_subst subst1 v) env in
        let t' = Types.generalize (Types.TypeMap.to_list env') t1 in (* WARNING: Don't like the fact that we need to convert the type env to a list *)
        let env'' = Types.extend_env env' i t' in

        let { expr = in_expr; t = t2; subst = subst2; rest = rest'; } = parse_ins rest env'' in

        {
            expr = Let (i, body, in_expr);
            t = t2;
            subst = Types.( *&* ) subst2  subst1;
            rest = rest';
        }
        
    | _ -> failwith "Token not recognized"
;;

(* TODO: Adapt this function to display type *)
let rec parse (program: Token.t list) (env: Types.env) =
    let { expr; t; subst; rest; } = parse_expr program env in
    match rest with
    | [] -> [(expr, t)]
    | l -> (expr, t) :: (parse l env) (* WARNING: Env is not updated after the parsing of an expression, which is obviously wrong. *)
;;

