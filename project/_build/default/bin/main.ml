open Token;;

let () =
    [LParen; Literal (Int 15); RParen] 
    |> Parser.parse
    |> Parser.show_ast
    |> print_string

