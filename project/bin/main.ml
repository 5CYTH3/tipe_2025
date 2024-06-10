open Token;;

let () =
    [LParen; Id "defun"; Id "add"; LParen; Id "x"; Id "y"; RParen; Id "x"; RParen] 
    |> Parser.parse
    |> Parser.show_ast
    |> print_string

