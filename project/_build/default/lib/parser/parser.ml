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

let rec parse_lambdas (program: Lexer.t) (env: Types.env): traversal =
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

and parse_arg (program: Lexer.t): string * Token.t list =
    match program with
    | Token.Id i :: t -> (i, t)
    | _ -> failwith "Unexpected token, expected an identifier"

and parse_lambda_body (program: Lexer.t) (env: Types.env): traversal =
    match program with
    | Dot :: t -> parse_expr t env
    | _ -> failwith "Expected lambda body definition (starting with a dot '.')"

(* TODO: Infer this function *)
and parse_app (f: traversal) (env: Types.env): traversal =
    let { expr; t = t1; subst; rest; } = f in 
    match rest with
    (* WARNING: Need to make sure that the first two cases receive the right environment *)
    | Id x :: t -> begin
        let env' = Types.TypeMap.map (fun v -> Types.apply_subst subst v) env in
        let t2 = Types.apply_env env' x in
        let tv = Types.fresh_tv () in
        let s3 = Types.unify (Types.apply_subst s2 t1) (Types.Abs (t2, tv)) in
        let trav = { 
            expr = App (expr, Var x); 
            t = (Types.apply_subst s3 tv); 
            subst = (Types.( *&* ) s3 (Types.( *&* ) s2 s1)); 
            rest = t; 
        } in
        parse_app trav env' in 
    end
    | Literal x :: t -> parse_app (App (f, match_literals x)) t env
    | LParen :: t -> f
    | _ -> f

and parse_ins (program: Lexer.t) (env: Types.env): traversal =
    match program with
    | In :: t -> parse_expr t env
    | _ -> failwith "No 'in' clause given. "

(* (expression, Token.t list) *)
and parse_expr (program: Lexer.t) (env: Types.env): traversal =
    match program with
    | Id i :: t -> parse_app t env (* not program but traversal *)
    | Lambda :: t -> parse_lambdas t env
    | Token.Literal l :: t -> 
        {
            expr = match_literals l;
            t = type_literals l;
            subst = Types.TypeMap.empty;
            rest = t;
        }  
    | LParen :: t -> begin
        let e = parse_expr t env in
        match e.rest with
        | RParen :: t' -> { expr = e.expr; t = e.t; subst = t.subst; rest = t'; } in
        | _ -> failwith "Unclosed parenthesis"
    end
    | Let :: Id i :: Assign :: t -> 
        let { expr = body; t = t1; subst = subst1; rest; } = parse_expr t env in
        let env' = Types.TypeMap.map (fun v -> Types.apply_subst subst1 v) env in
        let t' = Types.generalize (Types.TypeMap.to_list env') t1 in (* WARNING: Don't like the fact that we need to convert the type env to a list *)
        let env'' = Types.extend_env env' i t' in

        let { expr = in_expr; t = t2; subst = subst2; rest = rest'; } = parse_ins rest env'' in

        {
            expr = Let (i, body, in_expr);
            t = t2;
            subst = Types.( *&* ) subst2 subst1;
            rest = rest';
        }
    | RParen :: t -> failwith "Alone RParen. ?"
    | _ -> failwith "Token not recognized"
;;

(* TODO: Adapt this function to display type *)
let rec parse (program: Lexer.t) (env: Types.env) =
    let { expr; t; subst; rest; } = parse_expr program env in
    match rest with
    | [] -> [(expr, t)]
    | l -> (expr, t) :: (parse l env) (* WARNING: Env is not updated after the parsing of an expression, which is obviously wrong. *)
;;

