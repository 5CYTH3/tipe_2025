open Token;;

(* INFO: Do not try to run test. For now, they are copy-pasted from the `1.0-parser` branch, and are then made for the lisp-like grammar. *)
let () =
    [Lambda; Id "x"; Dot; Lambda; Id "y"; Dot; Id "+"; Id "x"; Id "y"] 
    |> Parser.parse
    |> Parser.show_ast
    |> print_string

