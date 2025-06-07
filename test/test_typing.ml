open Types
let testable_types = Alcotest.testable Types.pp ( = );;

let mock_test (expected_type: Types.t) ((expr: Parser.expr), (env: Types.env)) =
    let (t, _) = Types.infer_types env expr in
    Alcotest.check testable_types "same" expected_type t
;;

let test_2vars_1ret () =
    mock_test
        (TVar "t0" @-> (TVar "t1" @-> TVar "t0"))
        ((Abs ("x", Abs ("y", Var "x"))), Types.TypeMap.empty)

let lambda_exprs = [
    Alcotest.test_case "Nested Lambda Abs 1" `Slow test_2vars_1ret;
]


let () =
    Alcotest.run "Type inference" 
    [ 
        "Lambda Expressions", lambda_exprs;
        (*
        "Function Application", function_application;
        "Let Bindings", let_bindings_suite;
        *)
    ]
