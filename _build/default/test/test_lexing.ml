(* Whoops, no lexer here *)

let testable_program = Alcotest.testable Token.pp_program ( = );;

let mock_test expected_ast stream =
    let lexbuf = stream |> Lexing.from_string in
    let lexer = Lexer.collect_tokens lexbuf [] in
    Alcotest.check testable_program "same" expected_ast lexer
;;

let test_id_let () = 
    mock_test
        ([ Let; Id "id"; Assign; Lambda; Id "x"; Dot; Id "x"; In; Id "id" ])
        "let id = \\x. x in id"
;;

let test_function_app () =
    mock_test
        ([Let; Id "two"; Assign; Lambda; Id "f"; Dot; Lambda; Id "x"; Dot; Id "f"; Id "f"; Id "x"; In; Id "two"])
        "let two = \\f.\\x.f f x in two"


let let_bindings_suite = [
    Alcotest.test_case "ID Function" `Slow test_id_let;
    Alcotest.test_case "Application" `Slow test_function_app;
] 



let () =
    Alcotest.run "Lexer" 
    [ 
        "Let Bindings", let_bindings_suite;
    ]
