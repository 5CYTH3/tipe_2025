{
open Token (* Replace `Ast` with the module containing your token definitions *)
}

rule token = parse
    | [' ' '\t' '\n' '\r'] { token lexbuf } (* Skip whitespace *)
    | "let" { Let }
    | "\\" { Lambda }
    | "in" { In }
    | "." { Dot }
    | "=" { Assign }
    | "(" { LParen }
    | ")" { RParen }
    | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id {
            match id with
            | "true" -> Literal (Bool true)
            | "false" -> Literal (Bool false)
            | _ -> Id id
    }
    | ['0'-'9']+ as num { Literal (Int (int_of_string num)) }
    | '"' [^ '"' '\\']* '"' as str {
        let unescaped_str = String.sub str 1 (String.length str - 2) in
        Literal (Str unescaped_str)
    }
    | eof { raise End_of_file } (* End of file token *)
    | _ as x { failwith ("Unrecognized character" ^ (String.make 1 x)) }

{
let rec collect_tokens lexbuf acc =
    try
      let tok = token lexbuf in
      collect_tokens lexbuf (tok :: acc)
    with
    | End_of_file -> List.rev acc
    | Failure msg -> failwith msg
}
