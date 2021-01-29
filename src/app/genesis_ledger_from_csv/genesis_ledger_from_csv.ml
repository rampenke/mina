(* genesis_ledger_from_csv.ml -- create JSON-format genesis ledger from comma-separated-value data *)

open Core_kernel
open Async
open Mina_numbers

let slot_duration_ms =
  Consensus.Configuration.t
    ~constraint_constants:Genesis_constants.Constraint_constants.compiled
    ~protocol_constants:Genesis_constants.compiled.protocol
  |> Consensus.Configuration.slot_duration

(* a month = 30 days, for purposes of vesting *)
let slots_per_month = 30 * 24 * 60 * 60 * 1000 / slot_duration_ms

let runtime_config_account ~logger ~recipient ~wallet_pk ~amount
    ~initial_min_balance ~cliff_time_months ~cliff_amount ~unlock_frequency
    ~unlock_amount ~delegatee_pk_opt =
  [%log info] "Processing record for $recipient"
    ~metadata:[("recipient", `String recipient)] ;
  let pk = Some wallet_pk in
  let balance = Currency.Balance.of_string amount in
  let initial_minimum_balance =
    (* if omitted in the CSV, use balance *)
    match initial_min_balance with
    | "" ->
        balance
    | _ ->
        Currency.Balance.of_string initial_min_balance
  in
  let cliff_time =
    Global_slot.of_int (Int.of_string cliff_time_months * slots_per_month)
  in
  let cliff_amount = Currency.Amount.of_string cliff_amount in
  let vesting_period =
    match int_of_string unlock_frequency with
    | 0 ->
        Global_slot.of_int 1
    | 1 ->
        Global_slot.of_int slots_per_month
    | _ ->
        failwithf "Expected unlock frequency to be 0 or 1, got %s"
          unlock_frequency ()
  in
  let vesting_increment = Currency.Amount.of_string unlock_amount in
  let timing =
    Some
      { Runtime_config.Json_layout.Accounts.Single.Timed.initial_minimum_balance
      ; cliff_time
      ; cliff_amount
      ; vesting_period
      ; vesting_increment }
  in
  let delegate = delegatee_pk_opt in
  { Runtime_config.Json_layout.Accounts.Single.default with
    pk
  ; balance
  ; timing
  ; delegate }

let account_of_csv ~logger csv =
  (* delegatee is optional *)
  match String.split csv ~on:',' with
  | [ recipient
    ; wallet_pk
    ; amount
    ; initial_min_balance
    ; cliff_time_months
    ; cliff_amount
    ; unlock_frequency
    ; unlock_amount
    ; delegatee_pk ] ->
      runtime_config_account ~logger ~recipient ~wallet_pk ~amount
        ~initial_min_balance ~cliff_time_months ~cliff_amount ~unlock_frequency
        ~unlock_amount ~delegatee_pk_opt:(Some delegatee_pk)
  | [ recipient
    ; wallet_pk
    ; amount
    ; initial_min_balance
    ; cliff_time_months
    ; cliff_amount
    ; unlock_frequency
    ; unlock_amount ] ->
      runtime_config_account ~logger ~recipient ~wallet_pk ~amount
        ~initial_min_balance ~cliff_time_months ~cliff_amount ~unlock_frequency
        ~unlock_amount ~delegatee_pk_opt:None
  | _ ->
      failwithf "CSV line does not contain expected fields: %s" csv ()

let main ~csv_file ~output_file () =
  let logger = Logger.create () in
  let accounts, num_accounts =
    In_channel.with_file csv_file ~f:(fun in_channel ->
        [%log info] "Opened CSV file $csv_file"
          ~metadata:[("csv_file", `String csv_file)] ;
        let rec go accounts num_accounts =
          match In_channel.input_line in_channel with
          | Some line ->
              let account =
                try account_of_csv ~logger line
                with exn ->
                  [%log fatal] "Could not process record, error: $error"
                    ~metadata:
                      [ ("error", `String (Exn.to_string exn))
                      ; ("csv", `String line) ] ;
                  Core_kernel.exit 1
              in
              go (account :: accounts) (num_accounts + 1)
          | None ->
              (List.rev accounts, num_accounts)
        in
        (* skip first line *)
        let _headers = In_channel.input_line in_channel in
        go [] 0 )
  in
  [%log info] "Processed %d records, writing output JSON" num_accounts ;
  Out_channel.with_file output_file ~f:(fun out_channel ->
      let json =
        `List (List.map accounts ~f:Runtime_config.Accounts.Single.to_yojson)
      in
      Out_channel.output_string out_channel (Yojson.Safe.to_string json) ;
      Out_channel.newline out_channel ) ;
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
