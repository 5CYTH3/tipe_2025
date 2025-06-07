let testable_ast = Alcotest.testable Token.pp_ast ( = );;

let mock_test expected_ast stream =
    let lexbuf = stream |> Lexing.from_string in
    let lexer = Lexer.collect_tokens lexbuf [] in
    Alcotest.check testable_ast "same" expected_ast lexer
;;

let test_id_let () = 
    mock_test
        ([ Let; Id "id"; Assign; Lambda; Id "x"; Dot; Id "x"; In; Id "id" ])
        "let id = \\x. x in id"
;;


let let_bindings_suite = [
    Alcotest.test_case "ID Function" `Slow test_id_let;
] 



let () =
    Alcotest.run "Lexer" 
    [ 
        "Let Bindings", let_bindings_suite;
    ]
