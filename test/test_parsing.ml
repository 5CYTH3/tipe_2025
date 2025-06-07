open Token
open Parser

let testable_expr = Alcotest.testable Parser.pp_expr ( = );;

let mock_test (expected_expr: expr) lexer =
    let expr = Parser.parse lexer in
    Alcotest.check testable_expr "same" expected_expr expr
;;

let test_2vars_1ret () = 
    mock_test
        (Abs ("x", Abs ("y", Var "x")))
        [ Lambda; Id "x"; Dot; Lambda; Id "y"; Dot; Id "x" ]
;;

let test_3vars_1ret () =
    mock_test 
        (Abs ("x", Abs ("y", Abs("z", Var "y"))))
        [ Lambda; Id "x"; Dot; Lambda; Id "y"; Dot; Lambda; Id "z"; Dot; Id "y" ]
;;

let lambda_exprs = [
    Alcotest.test_case "Nested Lambda Abs 1" `Slow test_2vars_1ret;
    Alcotest.test_case "Nested Lambda Abs 2" `Slow test_3vars_1ret;
]

let test_fun_app_1 () =
    mock_test
        (App (Abs ("x", Var "x"), Literal (Int 1)))
        [ LParen; Lambda; Id "x"; Dot; Id "x"; RParen; Literal (Int 1) ]
;;

let test_fun_app_2 () =
    mock_test
        (App (Abs ("x", Abs ("y", Var "x")), Literal (Int 1)))
        [ LParen; Lambda; Id "x"; Dot; Lambda; Id "y"; Dot; Id "x"; RParen; Literal (Int 1) ]
;;

let function_application = [
    Alcotest.test_case "ID Function" `Slow test_fun_app_1;
    Alcotest.test_case "ID Function" `Slow test_fun_app_2;
]


let test_id_let () = 
    mock_test
        (Let ("id", Abs ("x", Var "x"), Var "id"))
        [Token.Let; Id "id"; Assign; Lambda; Id "x"; Dot; Id "x"; In; Id "id"]
;;

let test_add_let () =
    mock_test
        (
            Let ("add", 
                Abs ("x", 
                    Abs ("y", 
                        App (App (Var "+", Var "x"), Var "y"))),
                Var "add")
        )
        [
            Token.Let; Id "add"; Assign;
                Lambda; Id "x"; Dot;
                Lambda; Id "y"; Dot;
                Id "+"; Id "x"; Id "y";
            In; Id "add";
        ]


let let_bindings_suite = [
    Alcotest.test_case "ID Function" `Slow test_id_let;
    Alcotest.test_case "Add function" `Slow test_add_let;
] 

let () =
    Alcotest.run "Parser" 
    [ 
        "Lambda Expressions", lambda_exprs;
        "Function Application", function_application;
        "Let Bindings", let_bindings_suite;
    ]
