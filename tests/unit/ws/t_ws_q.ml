open Test_util

let apply_masking = Tiny_httpd_ws.Private_.apply_masking

let decode ?(mask_offset = 0) ~key b =
  let buf = Bytes.copy b in
  apply_masking ~mask_key:key ~mask_offset buf 0 (Bytes.length buf);
  buf

let ppb b = Printf.sprintf "%S" (Bytes.unsafe_to_string b)

let () =
  add_qcheck
  @@ QCheck.Test.make ~count:10_000
       Q.(
         triple
           (bytes_size (Gen.return 4))
           (option nat_small)
           (bytes_size Gen.(0 -- 6000))
         (* |> Q.add_stat ("b.size", fun (_k, b) -> Bytes.length b) *)
         |> Q.add_shrink_invariant (fun (k, _, _) -> Bytes.length k = 4))
       (fun (key, mask_offset, b) ->
         Q.assume (Bytes.length key = 4);
         let b2 = decode ~key ?mask_offset b in

         let is_zero =
           Bytes.equal key (Bytes.unsafe_of_string "\x00\x00\x00\x00")
         in

         if Bytes.length b >= 2 && not is_zero then (
           (* key must modify the byte vec *)
           let are_eq = Bytes.equal b b2 in
           if are_eq then
             Q.Test.fail_reportf "key=%s, expected different: b=%S b2=%S"
               (ppb key) (ppb b) (ppb b2)
         );

         let b3 = decode ~key ?mask_offset b2 in
         assert (Bytes.equal b b3);
         true)

let () = run_qcheck_and_exit ()
