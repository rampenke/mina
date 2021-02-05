(* archive_node.ml -- generate data from archive node in cloud *)

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

  let run network t =
    let open Malleable_error.Let_syntax in
    Core_kernel.eprintf "NUM BLOCK PRODUCERS: %d\n%!"
      (List.length (Network.block_producers network)) ;
    Core_kernel.eprintf "NUM ARCHIVE NODES: %d\n%!"
      (List.length (Network.archive_nodes network)) ;
    let block_producer = Caml.List.nth (Network.block_producers network) 0 in
    let%bind () =
      wait_for t (Wait_condition.node_to_initialize block_producer)
    in
    let node = Core_kernel.List.nth_exn (Network.block_producers network) 0 in
    let logger = Logger.create () in
    (* wait for initialization *)
    let%bind () = wait_for t (Wait_condition.node_to_initialize node) in
    [%log info] "send_payment_test: done waiting for initialization" ;
    (* same keypairs used by Coda_automation to populate the ledger *)
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
