open Types
open Token
open Fused

let testable_expr = Alcotest.testable Expr.pp ( = );;
let testable_type = Alcotest.testable Types.pp ( = );; 
let testable = Alcotest.pair testable_expr testable_type;;

let mock_test (expected_expr, expected_type) (lexer, ctx) =
    let expr, t = parse lexer ctx in
    Alcotest.check testable "same pair" (expected_expr, expected_type) (expr, t)
;;

let test_2vars_1ret () = 
    mock_test
        (Abs ("x", Abs ("y", Var "x")), TVar "t0" @-> TVar "t1" @-> TVar "t0")
        ([ Lambda; Id "x"; Dot; Lambda; Id "y"; Dot; Id "x" ], TypeMap.empty)
;;

let test_3vars_1ret () =
    mock_test 
        (Abs ("x", Abs ("y", Abs("z", Var "y"))), TVar "t0" @-> TVar "t1" @-> TVar "t2" @-> TVar "t1")
        ([ Lambda; Id "x"; Dot; Lambda; Id "y"; Dot; Lambda; Id "z"; Dot; Id "y" ], TypeMap.empty)
;;

let lambda_exprs = [
    Alcotest.test_case "Nested Lambda Abs 1" `Quick test_2vars_1ret;
    Alcotest.test_case "Nested Lambda Abs 2" `Quick test_3vars_1ret;
]

let test_fun_app_1 () =
    mock_test
        (App (Abs ("x", Var "x"), Literal (Int 1)), Int)
        ([ LParen; Lambda; Id "x"; Dot; Id "x"; RParen; Literal (Int 1) ], TypeMap.empty)
;;

let test_fun_app_2 () =
    mock_test
        (App (Abs ("x", Abs ("y", Var "x")), Literal (Int 1)), TVar "t1" @-> Int)
        ([ LParen; Lambda; Id "x"; Dot; Lambda; Id "y"; Dot; Id "x"; RParen; Literal (Int 1) ], TypeMap.empty)
;;

let function_application = [
    Alcotest.test_case "ID Function" `Quick test_fun_app_1;
    Alcotest.test_case "ID Function" `Quick test_fun_app_2;
]


let test_id_let () = 
    mock_test
        (Let ("id", Abs ("x", Var "x"), Var "id"), Forall ("t0", Abs(TVar "t0", TVar "t0")))
        (
            [Token.Let; Id "id"; Assign; Lambda; Id "x"; Dot; Id "x"; In; Id "id"],
            TypeMap.empty
        )
;;

let test_add_let () =
    mock_test
        (
            Let ("add", 
                Abs ("x", 
                    Abs ("y", 
                        App (App (Var "+", Var "x"), Var "y"))),
                Var "add"),
            (Types.Abs (Int, Abs (Int, Int)))
        )
        (
            [Token.Let; Id "add"; Assign;
                    Lambda; Id "x"; Dot;
                    Lambda; Id "y"; Dot;
                    Id "+"; Id "x"; Id "y";
                In; Id "add";
            ],
            TypeMap.of_list [ ("+", Types.Abs (Int, Abs (Int, Int))) ]
        )


let let_bindings_suite = [
    Alcotest.test_case "ID Function" `Quick test_id_let;
    Alcotest.test_case "Add function" `Quick test_add_let;
] 



let () =
    Alcotest.run "Parser" 
    [ 
        "Lambda Expressions", lambda_exprs;
        "Function Application", function_application;
        "Let Bindings", let_bindings_suite;
    ]
