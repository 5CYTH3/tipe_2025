open Cmdliner


let show_program (e, t, ctx) = 
    Printf.printf "\nCTX: %s\nTYPE: %s\nEXPR:\n%s\n\n"
        (Types.show_typemap ctx)
        (Types.show t)
        (Parser.show_expr e)
;;

let from_file f b_time =
    let ic = open_in f in
    let lexbuf = really_input_string ic (in_channel_length ic) |> Lexing.from_string in
    let lexer = Lexer.collect_tokens lexbuf [] in
    (* print_string @@ Parser.show_program lexer; *)
    let start = Sys.time () in
    let res = Parser.parse lexer Types.TypeMap.empty in
    let stop = Sys.time () in
    let elapsed = stop -. start in
    if b_time then ( 
        print_float elapsed
    ) else show_program res
;;

let from_str s b_time = 
    let lexbuf = s |> Lexing.from_string in
    let lexer = Lexer.collect_tokens lexbuf [] in
    let start = Sys.time () in
    let res = Parser.parse lexer Types.TypeMap.empty in
    let stop = Sys.time () in
    let elapsed = stop -. start in
    if b_time then ( 
        print_float elapsed
    ) else show_program res

;;

let main t o string_opt file_opt =
    match (t, o) with
    | true, true -> 
        Printf.eprintf "Error: only one of -t or -o can be specified\n";
        `Error (false, "Conflicting flags")
    | _ -> begin
        match string_opt, file_opt with
        | None, None ->
            Printf.eprintf "Error: Provide either a string or --file option\n";
            `Error (false, "Missing argument")
        | Some s, None ->
            from_str s t;
            `Ok ()
        | None, Some f ->
            from_file f t;
            `Ok ()
        | Some _, Some _ ->
            Printf.eprintf "Error: Provide either a string or --file option, not both\n";
            `Error (false, "Conflicting arguments")
    end

let flag_t =
  let doc = "Option t" in
  Arg.(value & flag & info ["t"] ~doc)

let flag_o =
  let doc = "Option o" in
  Arg.(value & flag & info ["o"] ~doc)

let string_arg =
  let doc = "Input string" in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"STRING" ~doc)

let file_arg =
  let doc = "Input file" in
  Arg.(value & opt (some string) None & info ["file"] ~docv:"FILE" ~doc)

let cmd =
  let info = Cmd.info "build" ~doc:"Example CLI with flags and input" in
  Cmd.v info
    Term.(ret (const main $ flag_t $ flag_o $ string_arg $ file_arg))

let () = 
    exit @@ Cmd.eval cmd


