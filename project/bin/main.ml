open Token;;

let ctx = Types.TypeMap.of_list [
    "+", Types.Abs (Types.Int, Types.Abs (Types.Int, Types.Int)); 
    "y", Types.Int
    (* App is left-assoc but TAbs is right-assoc *)
]

(* INFO: Do not try to run test. For now, they are copy-pasted from the `1.0-parser` branch, and are then made for the lisp-like grammar. *)
let () =
    (* 
    WARNING: Weird thing with this example: the type is not `Forall t -> t` but only `(tvar t) -> (tvar t)`

    Parser.parse [
        Lambda; Id "x"; Dot; Id "x";
    ] ctx
    *)
    (*
    Parser.parse [
        Id "+"; Literal (Int 4); Literal (Int 3);
    ] ctx
    *)
    (*
    Parser.parse [
        Let; Id "five"; Assign; Literal (Int 5); In; Id "five";
    ] ctx
    *)
    (*
    Parser.parse [
        LParen; Lambda; Id "x"; Dot; Id "+"; Id "x"; Literal (Int 1); RParen; Literal (Int 4);
    ] ctx
    *)
    (*
    Parser.parse [
        Let; Id "add"; Assign; 
            Lambda; Id "x"; Dot;
            Lambda; Id "y"; Dot;
            Id "+"; Id "x"; Id "y";
        In; Id "add"; Literal (Int 4); Literal (Int 3);
    ] ctx
    *)
    (*
    Parser.parse [
        Let; Id "id"; Assign; Lambda; Id "x"; Dot; Id "x"; In; Id "id"
    ] ctx
    *)
    Parser.parse [
        LParen; Lambda; Id "x"; Dot; Id "x"; RParen; Id "y"
    ] ctx
    |> (fun (e, t, ctx) -> begin
        Printf.printf "CTX: %s\nTYPE: %s\nEXPR:\n%s\n\n"
        (Types.show_typemap ctx)
        (Types.show t)
        (Parser.show_expr e)
    end)

