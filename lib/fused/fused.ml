open Expr;;

type traversal = {
    expr: Expr.t;
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

let fresh_id (id: string) (env: Types.env): Types.t * Types.env = 
    let tv = Types.fresh_tv () in
    let env' = Types.extend_env env id tv in
    (tv, env')
;;

let rec parse_expr (program: Token.t list) (env: Types.env): traversal =
    match program with
    | Lambda :: Id arg :: Dot :: rest ->
        let (tv, env') = fresh_id arg env in
        let { expr = body; t; subst; rest = rest'; } = parse_expr rest env' in 

        {
            expr = Abs (arg, body);
            t = Types.Abs (Types.apply_subst subst tv, t);
            subst;
            rest = rest';
        }
    | Let :: _ -> parse_let program env
    | _ -> parse_app program env
and parse_let (program: Token.t list) (env: Types.env): traversal =
    match program with
    | Let :: Id i :: Assign :: t -> 
        let open Types in
        let { expr = body; t = t1; subst = subst1; rest; } = parse_expr t env in
        let t' = generalize env (apply_subst subst1 t1) in (* Get the type of the let def *)
        let env' = extend_env env i t' in (* Add the typesig of the let-expr to the ctx *)

        let { expr = in_expr; t = t2; subst = subst2; rest = rest'; } = parse_ins rest env' in

        {
            expr = Let (i, body, in_expr);
            t = t2;
            subst = subst2 *&* subst1;
            rest = rest';
        }
    | _ -> failwith "Expected `Let`."

and parse_ins (program: Token.t list) (env: Types.env): traversal =
    match program with
    | In :: t -> parse_expr t env
    | _ -> failwith "No 'in' clause given. "

and parse_parenthesized (program: Token.t list) (env: Types.env): traversal =
    match program with
    | LParen :: t -> begin
        let e = parse_expr t env in
        match e.rest with
        | RParen :: t' -> { expr = e.expr; t = e.t; subst = e.subst; rest = t'; }
        | _ -> failwith "Unclosed parenthesis"
    end
    | _ -> failwith "Expected LParen."


and parse_app (program: Token.t list) (env: Types.env): traversal = 
    let open Types in

    (* WARNING: I bet this is hideous complexity-wise *)
    let rec aux (lhs: traversal) = 
        let { expr = lhs_expr; t = t1; rest; subst = s1; } = lhs in
        match rest with
        | [] | In :: _ | RParen :: _ -> lhs
        | _ -> begin
            let { expr = rhs_expr; t = t2; subst = s2; rest = rest'; } = parse_term rest env in
            let tv = fresh_tv () in
            let s3 = unify (apply_subst s2 t1) (Abs (t2, tv)) in (* VERY HEAVY *)
            let f_subst = s3 *&* s2 *&* s1 in

            let res = {
                expr = App (lhs_expr, rhs_expr);
                t = apply_subst f_subst tv; 
                subst = f_subst;
                rest = rest';
            } in
            aux res
        end
    in 
    let initial = parse_term program env in
    aux initial
(* (expression, Token.t list) *)
and parse_term (program: Token.t list) (env: Types.env): traversal =
    match program with
    | Id x :: rest -> 
        let t = Types.apply_env env x in 
        { 
            expr = Var x; 
            t;
            subst = Types.TypeMap.empty; 
            rest;
        }
    | Token.Literal l :: t -> 
        {
            expr = match_literals l;
            t = type_literals l;
            subst = Types.TypeMap.empty;
            rest = t;
        }  
    | LParen :: _ -> parse_parenthesized program env
    | RParen :: _ -> failwith "Alone RParen. ?"
    | _ -> failwith "Unexpected token"
;;

let parse (program: Token.program) (env: Types.env) =
    Types.reset_tv_counter ();
    let { expr; t; subst = _; rest = _; } = parse_expr program env in
    (* A lambda-calculus program is ONE expression (nested let-ins for declarations) *)
    (expr, t)
;;

