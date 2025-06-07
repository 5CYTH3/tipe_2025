let time f l ctx =
    let start = Sys.time () in
    let _ = f l ctx in
    let stop = Sys.time () in
    stop -. start

let list_to_csv l =
    List.map (fun (n, fused, classic) -> [string_of_int n; string_of_float fused; string_of_float classic]) l

