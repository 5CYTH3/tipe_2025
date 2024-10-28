
(*
open Parser;;
open Token;;
let parser_testable = Alcotest.testable Parser.pp_expr (fun x y -> x = y) 

(* TODO: Test individual values such as atoms *)

(* Template for testing lists used in specialized tests *)
let mock_test_parsing_lists p expected =
    let got = parse p in
    Alcotest.(check (Alcotest.list parser_testable)) "same lists" got expected
;;

(* Only test lists of atomic ints   *)
(* It here tests the sequence `(0)` *)
let test_int_list () = 
    mock_test_parsing_lists
        [LParen; Literal (Int 0); RParen]
        [Parser.List [Atom (Int 0)]]
;;

(* Only test lists of any type of atoms               *)
(* It here tests the sequence `(0 "Hello"  true add)` *)
let test_any_list () =
    mock_test_parsing_lists
        [LParen; Literal (Int 0); Literal (Str "Hello"); Literal (Bool true); Id "add"; RParen]
        [List [Atom (Int 0); Atom (Str "Hello"); Atom (Bool true); Atom (Id "add")]]
;;


(* Test list of atoms and lists                        *)
(* It here tests the sequence `((4 1) ("Hello") true)` *)
let test_list_of_lists () =
    mock_test_parsing_lists 
    [LParen; LParen; Literal (Int 4); Literal (Int 1); RParen; LParen; Literal (Str "Hello"); RParen; Literal (Bool true); RParen]
    [List [List [Atom (Int 4); Atom (Int 1)]; List [Atom (Str "Hello")]; Atom (Bool true)]]
;;

let lists_suite = [
    Alcotest.test_case "List of ints" `Quick test_int_list;
    Alcotest.test_case "List of any type" `Quick test_any_list; 
    Alcotest.test_case "List of lists and any" `Quick test_list_of_lists;
]


(* Test functions that have no parameters *)
(* Tested sequence : (defun x () 3) *)
let test_no_args_fn () =
    let got = parse [LParen; Id "defun"; Id "x"; LParen; RParen; Literal (Int 3); RParen] in
    Alcotest.(check (Alcotest.list parser_testable)) "same lists" got [Function ("x", [], Atom (Int 3))]
;;

(* Test functions that have arguments *)
(* Tested sequence : (defun x (y z) z) *)
let test_args_fn () =
    let got = parse [LParen; Id "defun"; Id "x"; LParen; Id "y"; Id "z"; RParen; Id "z"; RParen] in
    Alcotest.(check (Alcotest.list parser_testable)) "same lists" got [Function ("x", ["y"; "z"], Atom (Id "z"))];;

(* Test function that got a function definition in its body *)
(* (defun x () (defun y () 3)) *)
let test_nested_fn () = 
    let got = parse [LParen; Id "defun"; Id "x"; LParen; RParen; LParen; Id "defun"; Id "y"; LParen; RParen; Literal (Int 3); RParen; RParen] in
    Alcotest.(check (Alcotest.list parser_testable)) "same lists" got [Function ("x", [], Function ("y", [], Atom (Int 3)))]
;;



let function_suite = [
    Alcotest.test_case "Function with no argument" `Quick test_no_args_fn;
    Alcotest.test_case "Function with arguments" `Quick test_args_fn;
    Alcotest.test_case "Two nested functions" `Quick test_nested_fn;
]

let () =
    Alcotest.run "Parser" 
    [ 
        "Lists", lists_suite;
        "Functions", function_suite
    ]
*)
