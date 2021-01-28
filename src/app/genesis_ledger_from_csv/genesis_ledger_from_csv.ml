(* genesis_ledger_from_csv.ml -- create JSON-format genesis ledger from comma-separated-value data *)

open Core_kernel
open Async

let json_of_csv csv =
  (* delegatee is optional *)
  match String.split csv ~on:',' with
  | [ recipient_pk
    ; wallet_pk
    ; amount
    ; cliff_time_months
    ; cliff_amount
    ; unlock_frequency
    ; unlock_amount
    ; delegatee_pk ] ->
      ()
  | [ recipient_pk
    ; wallet_pk
    ; amount
    ; cliff_time_months
    ; cliff_amount
    ; unlock_frequency
    ; unlock_amount ] ->
      ()
  | _ ->
      failwithf "CSV line does not contain expected fields: %s" csv ()

let main ~csv_file ~output_file:_ () =
  let logger = Logger.create () in
  let _jsons =
    In_channel.with_file csv_file ~f:(fun in_channel ->
        [%log info] "Opened CSV file $csv_file"
          ~metadata:[("csv_file", `String csv_file)] ;
        let rec go jsons =
          match In_channel.input_line in_channel with
          | Some line ->
              let json = json_of_csv line in
              go (json :: jsons)
          | None ->
              List.rev jsons
        in
        go [] )
  in
  return ()

let () =
  Command.(
    run
      (let open Let_syntax in
      async ~summary:"Write blocks to an archive database"
        (let%map output_file =
           Param.flag "output-file"
             ~doc:
               "PATH File that will contain the genesis ledger in JSON format"
             Param.(required string)
         and csv_file =
           Param.flag "csv-file"
             ~doc:
               "PATH File containing genesis ledger in comma-separated-value \
                format"
             Param.(required string)
         in
         main ~csv_file ~output_file)))
