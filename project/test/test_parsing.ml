open Types
open Token
open Parser

let testable_expr = Alcotest.testable Parser.pp_expr ( = );;
let testable_type = Alcotest.testable Types.pp ( = );; let testable = Alcotest.pair testable_expr testable_type;;

let mock_test (expected_expr, expected_type) (lexer, ctx) =
    let expr, t, _ = Parser.parse lexer ctx in
    Alcotest.check testable "same pair" (expected_expr, expected_type) (expr, t)
;;

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
        "Let Bindings", let_bindings_suite;
    ]
