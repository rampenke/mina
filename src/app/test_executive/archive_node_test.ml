(* archive_node.ml -- generate data from archive node in cloud *)

open Core_kernel
open Integration_test_lib

module Make (Inputs : Intf.Test.Inputs_intf) = struct
  open Inputs
  open Engine
  open Dsl

  type network = Network.t

  type node = Network.Node.t

  type dsl = Dsl.t

  let config =
    let open Test_config in
    { default with
      (* a few block producers, where few = 4 *)
      block_producers=
        [ {balance= "4000"; timing= Untimed}
        ; {balance= "9000"; timing= Untimed}
        ; {balance= "8000"; timing= Untimed}
        ; {balance= "17000"; timing= Untimed} ]
    ; num_snark_workers= 0 }

  let expected_error_event_reprs = []

  (* number of minutes to let the network run, after initialization *)
  let runtime_min = 15.

  let run network t =
    let open Malleable_error.Let_syntax in
    Core_kernel.eprintf "NUM BLOCK PRODUCERS: %d\n%!"
      (List.length (Network.block_producers network)) ;
    Core_kernel.eprintf "NUM ARCHIVE NODES: %d\n%!"
      (List.length (Network.archive_nodes network)) ;
    let logger = Logger.create () in
    let block_producers = Network.block_producers network in
    [%log info] "archive node test: waiting for block producers to initialize" ;
    let%bind () =
      Malleable_error.List.iter block_producers ~f:(fun bp ->
          wait_for t (Wait_condition.node_to_initialize bp) )
    in
    [%log info] "archive node test: waiting for archive node to initialize" ;
    let archive_node = List.hd_exn @@ Network.archive_nodes network in
    let%bind () =
      wait_for t (Wait_condition.node_to_initialize archive_node)
    in
    [%log info] "archive node test: running network for %0.1f minutes"
      runtime_min ;
    let%bind.Async.Deferred.Let_syntax () =
      Async.after (Time.Span.of_min runtime_min)
    in
    [%log info] "archive node test: done running network" ;
    let keypairs = Lazy.force Mina_base.Sample_keypairs.keypairs in
    (* send the payment *)
    let sender, _sk1 = keypairs.(0) in
    let receiver, _sk2 = keypairs.(1) in
    let amount = Currency.Amount.of_int 200_000_000 in
    let fee = Currency.Fee.of_int 10_000_000 in
    let%bind () =
      Network.Node.send_payment ~logger node ~sender ~receiver ~amount ~fee
    in
    (* confirm payment *)
    let%map () =
      wait_for t
        (Wait_condition.payment_to_be_included_in_frontier ~sender ~receiver
           ~amount)
    in
    [%log info] "send_payment_test: succesfully completed"
end
