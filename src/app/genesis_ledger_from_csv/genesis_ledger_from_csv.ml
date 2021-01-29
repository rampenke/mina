(* genesis_ledger_from_csv.ml -- create JSON-format genesis ledger from comma-separated-value data *)

open Core_kernel
open Async
open Signature_lib
open Mina_numbers

let slot_duration_ms =
  Consensus.Configuration.t
    ~constraint_constants:Genesis_constants.Constraint_constants.compiled
    ~protocol_constants:Genesis_constants.compiled.protocol
  |> Consensus.Configuration.slot_duration

let slots_per_month = 30 * 24 * 60 * 60 * 1000 / slot_duration_ms

let runtime_config_account ~recipient ~wallet_pk ~amount ~cliff_time_months
    ~cliff_amount ~unlock_frequency ~unlock_amount ~delegatee_pk_opt =
  let pk_of_string = Public_key.Compressed.of_base58_check in
  let pk = pk_of_string wallet_pk in
  let balance = Currency.Balance.of_string amount in
  let cliff_time = Global_slot.of_int (cliff_time_months * slots_per_month) in
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
    { Runtime_config.Json_layout.Accounts.Single.Timed.initial_minimum_balance
    ; cliff_time
    ; cliff_amount
    ; vesting_period
    ; vesting_increment }
  in
  let delegate = Option.map delegatee_pk_opt ~f:pk_of_string in
  { Runtime_config.Json_layout.Accounts.Single.default with
    pk
  ; balance
  ; timing
  ; delegate }

let json_of_csv ~logger csv =
  (* delegatee is optional *)
  match String.split csv ~on:',' with
  | [ recipient
    ; wallet_pk
    ; amount
    ; cliff_time_months
    ; cliff_amount
    ; unlock_frequency
    ; unlock_amount
    ; delegatee_pk ] ->
      ()
  | [ recipient
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
              let json = json_of_csv ~logger line in
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
