open Cmdliner


(*
let show_program (e, t) = 
    Printf.printf "\nTYPE: %s\nEXPR:\n%s\n\n"
        (Types.show t)
        (Expr.show e)
;;
*)

(*
let from_file parser f b_time =
    let ic = open_in f in
    let lexbuf = really_input_string ic (in_channel_length ic) |> Lexing.from_string in
    let lexer = Lexer.collect_tokens lexbuf [] in
    let start = Sys.time () in
    let res = parser lexer Types.TypeMap.empty in
    let stop = Sys.time () in
    let elapsed = stop -. start in
    if b_time then ( 
        print_float elapsed
    ) else show_program res
;;

let from_str parser s b_time = 
    let lexbuf = s |> Lexing.from_string in
    let lexer = Lexer.collect_tokens lexbuf [] in
    let start = Sys.time () in
    let res = parser lexer Types.TypeMap.empty in
    let stop = Sys.time () in
    let elapsed = stop -. start in
    if b_time then ( 
        print_float elapsed
    ) else show_program res

;;
*)

(* The function to run for "bench" *)
let bench_cmd ints: [ `Ok of unit | `Error of bool * string | `Help of Manpage.format * string option ] =
    Printf.printf "Running bench with inputs: %s\n"
        (String.concat ", " (List.map string_of_int ints));
    Benchmarks.Church_num.run_on_list ints 5 |> Benchmarks.Church_num.show_data |> print_string;
    `Ok ()

let ints_arg =
  let doc = "List of integers" in
  Arg.(value & pos_all int [] & info [] ~docv:"INT" ~doc)

let bench_cmd_term =
  let info = Cmd.info "bench" ~doc:"Run benchmarks with a list of integers" in
  Cmd.v info Term.(ret (const bench_cmd $ ints_arg))

let main_cmd =
    print_string "kappaprankex";
  let info = Cmd.info "mycli" ~doc:"Example CLI with subcommand bench" in
  Cmd.group info [ bench_cmd_term ]

let () = exit @@ Cmd.eval main_cmd
