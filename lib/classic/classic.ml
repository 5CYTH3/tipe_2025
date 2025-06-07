open Expr;;
open Token;;

type traversal = {
    expr: Expr.t;
    rest: Token.program;
}

let match_literals = function
    | Token.Str s -> Expr.Literal (Expr.Str s)
    | Token.Int i -> Expr.Literal (Expr.Int i)
    | Token.Bool b -> Expr.Literal(Expr.Bool b)
;;

let rec parse_expr (program: program): traversal =
    match program with
    | Lambda :: Id arg :: Dot :: rest ->
        let { expr = body; rest = rest'; } = parse_expr rest in 

        {
            expr = Abs (arg, body);
            rest = rest';
        }
    | Let :: _ -> parse_let program
    | _ -> parse_app program
and parse_let (program: program): traversal =
    match program with
    | Let :: Id i :: Assign :: t -> 
        let { expr = body; rest; } = parse_expr t in

        let { expr = in_expr; rest = rest'; } = parse_ins rest in

        {
            expr = Let (i, body, in_expr);
            rest = rest';
        }
    | _ -> failwith "Expected `Let`."

and parse_ins (program: program): traversal =
    match program with
    | In :: t -> parse_expr t
    | _ -> failwith "No 'in' clause given. "

and parse_parenthesized (program: program): traversal =
    match program with
    | LParen :: t -> begin
        let e = parse_expr t in
        match e.rest with
        | RParen :: t' -> { expr = e.expr; rest = t'; }
        | _ -> failwith "Unclosed parenthesis"
    end
    | _ -> failwith "Expected LParen."


and parse_app (program: program): traversal = 
    let rec aux (lhs: traversal) = 
        let { expr = lhs_expr; rest; } = lhs in

        match rest with
        | [] | In :: _ | RParen :: _ -> lhs
        | _ -> begin
            let { expr = rhs_expr; rest = rest'; } = parse_term rest in
            
            let res = {
                expr = App (lhs_expr, rhs_expr);
                rest = rest';
            } in
            aux res
        end
    in 
    let initial = parse_term program in
    aux initial

(* (expression, Token.t list) *)
and parse_term (program: program): traversal =
    match program with
    | Id x :: rest -> 
        { 
            expr = Var x; 
            rest;
        }
    | Token.Literal l :: t -> 
        {
            expr = match_literals l;
            rest = t;
        }  
    | LParen :: _ -> parse_parenthesized program
    | RParen :: _ -> failwith "Alone RParen. ?"
    | _ -> failwith "Unexpected token"
;;

let parse (program: program) =
    let { expr; rest = _; } = parse_expr program in
    (* A lambda-calculus program is ONE expression (nested let-ins for declarations) *)
    expr
;;

let type_of_literal = function
    | Expr.Str _ -> Types.Str
    | Expr.Bool _ -> Types.Bool
    | Expr.Int _ -> Types.Int
;;

let rec infer_types_aux env e cont = 
    let open Types in
    match e with
    | Expr.Literal l ->
        let t = type_of_literal l in
        cont (t, TypeMap.empty)
    | Var x ->
        let t = apply_env env x in
        cont (t, TypeMap.empty)
    | Abs (x, e) ->
        let t = fresh_tv () in
        let env' = extend_env env x t in
        infer_types_aux env' e (fun (t', s) ->
            let t_res = (apply_subst s t) @-> t' in
            cont (t_res, s)
        )
    | App (e1, e2) ->
        infer_types_aux env e1 (fun (t1, s1) ->
            let env' = apply_subst_to_env s1 env in
            infer_types_aux env' e2 (fun (t2, s2) ->
                let tv = fresh_tv () in
                let s3 = unify (apply_subst s2 t1) (t2 @-> tv) in
                let f_subst = s3 *&* s2 *&* s1 in
                cont (apply_subst f_subst tv, f_subst)
            )
        )
    | Let (x, e1, e2) ->
        infer_types_aux env e1 (fun (t1, s1) ->
            let t' = generalize env (apply_subst s1 t1) in
            let env' = extend_env env x t' in
            infer_types_aux env' e2 (fun (t2, s2) ->
                cont (t2, s2 *&* s1)
            )
        )
;;

let infer_types env e = 
  infer_types_aux env e (fun res -> res)

let run (p: program) (env: Types.env) =
    let ast = parse p in
    let t, _ = infer_types env ast in
    (ast, t)
;;
