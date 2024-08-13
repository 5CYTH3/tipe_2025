open Token;;

let ctx = Types.TypeMap.of_list [
    "+", Types.Abs (Types.Int, Types.Abs (Types.Int, Types.Int));
]

(* INFO: Do not try to run test. For now, they are copy-pasted from the `1.0-parser` branch, and are then made for the lisp-like grammar. *)
let () =
    [Lambda; Id "x"; Dot; Lambda; Literal (Int 3)] 
    |> Parser.parse
    |> Parser.show_ast
    |> print_string

