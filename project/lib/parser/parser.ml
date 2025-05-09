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

type traversal = {
    expr: expr;
    rest: Token.t list;
}

type program = Token.t list
[@@deriving show];;

let match_literals = function
    | Token.Str s -> Literal (Str s)
    | Token.Int i -> Literal (Int i)
    | Token.Bool b -> Literal(Bool b)
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
        | [] -> lhs
        | In :: _ | RParen :: _ -> lhs
        | _ -> begin
            let { expr = rhs_expr; rest = rest'; } = parse_term rest in
            
            let res = {
                expr = App (lhs_expr, rhs_expr);
                rest = rest';
            } in
            aux res
        end
    in aux @@ parse_term program

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
