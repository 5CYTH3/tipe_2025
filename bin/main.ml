let ctx = Types.TypeMap.of_list [
    "add", Types.Abs (Types.Int, Types.Abs (Types.Int, Types.Int)); 
    "y", Types.Int
    (* App is left-assoc but TAbs is right-assoc *)
]

let from_file f =
    let ic = open_in f in
    let lexbuf = really_input_string ic (in_channel_length ic) |> Lexing.from_string in
    let lexer = Lexer.collect_tokens lexbuf [] in
    (* print_string @@ Parser.show_program lexer; *)
    let start = Sys.time () in
    let (t, _) = Parser.parse lexer |> Types.infer_types ctx in
    let stop = Sys.time () in
    let elapsed = stop -. start in
    print_float elapsed;
    t
;;

(*
let from_str s = 
    let lexbuf = s |> Lexing.from_string in
    let lexer = Lexer.collect_tokens lexbuf [] in
    (* print_string @@ Parser.show_program lexer; *)
    Parser.parse lexer ctx
;;

let show_program (e, t, ctx) = 
    Printf.printf "\nCTX: %s\nTYPE: %s\nEXPR:\n%s\n\n"
        (Types.show_typemap ctx)
        (Types.show t)
        (Parser.show_expr e)
;;
*)

let () =
    from_file "test/main2.lc" |> ignore
    (* |> show_program *)

