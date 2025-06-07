(* Generate the Church numeral expression using your grammar *)
let church_expr n =
    let rec build_term i =
        if i = 0 then "x"
        else "f (" ^ build_term (i - 1) ^ ")"
    in
    "\\f.\\x." ^ build_term n

(* Generate a let-binding for the nth Church numeral *)
let generate_let_binding i =
    "let _" ^ string_of_int i ^ " = " ^ church_expr i ^ " in"

(* Generate the whole program with a final expression reference *)
let generate_program n =
    let buffer = Buffer.create (n * 50) in
    for i = 0 to n - 1 do
        Buffer.add_string buffer (generate_let_binding i);
        Buffer.add_char buffer '\n'
    done;
    (* Reference the last Church numeral *)
    Buffer.add_string buffer ("_" ^ string_of_int (n - 1) ^ "\n");
    Buffer.contents buffer

let run_bench n: float * float = 
    print_string "run_bench has been called";
    let program = generate_program n in
    let lexbuf = Lexing.from_string program in
    let lexer = Lexer.collect_tokens lexbuf [] in
    let fused_time = Utils.time Fused.parse lexer Types.TypeMap.empty in
    let classic_time = Utils.time Classic.run lexer Types.TypeMap.empty in
    fused_time, classic_time

(* Run k times on a sample of n nmumerals *)
let run_mean n k =
    print_string "run_mean has been called";
    let rec aux i (fused_acc, classic_acc) =
        if i = 0 then (fused_acc, classic_acc)
        else
            let fused, classic = run_bench n in
            aux (i - 1) (fused_acc +. fused, classic_acc +. classic)
    in let (fused, classic) = aux k (0.0, 0.0) in
    (fused /. float_of_int k, classic /. float_of_int k)
;;

type data = (int * float * float) list
[@@deriving show];;

(* Run on the list and return a (a * b * c) list where :
 * `a` is the number of numerals
 * `b` is the mean on k values of times of fused
 * `c` is the mean on k values of times of classic
 *)
let run_on_list l k =
    List.map (fun i -> let x, y = run_mean i k in i, x, y) l
;;
