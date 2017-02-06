open Treedoc
open Itreedoc

module Vpst = Vpst(struct
                     include Tc.String
                     module Path = Irmin.Path.String_list
                     let to_string t = t
                     let merge _ = failwith "String cannot be merged"
                   end)

let (>>=) = Vpst.bind

let bob_f : unit Vpst.t = 
  let open TreeDoc in
  Vpst.get_latest_version () >>= fun t0 -> 
  (*
   * Bob thinks that the fox that jumped is rather wheatish 
   * brown in color. Inserts "wheatish" before "brown".
   *)
  let t0' = insert t0 [L;L;R] "wheatish" in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Bob syncs with Alice. Gets nothing new. *)
  Vpst.sync_next_version ~v:t0' >>= fun t1 ->
  (*
   * Bob wants the describe this statement as a "thought".
   *)
  let t1' = insert t1 [L;L;L;L] "thought:" in
  let _ = Printf.printf "Bob (before syncing with Alice): \n" in
  let _ = print_doc t1' in
  (* Bob syncs with Alice again *)
  Vpst.liftLwt @@ Lwt_unix.sleep 1.0 >>= fun () ->
  Vpst.sync_next_version ~v:t1' >>= fun t2 ->
  let _ = Printf.printf "Bob (after syncing with Alice): \n" in
  let _ = print_doc t2 in
  Vpst.return ()
  
let alice_f : unit Vpst.t = 
  let open TreeDoc in 
  Vpst.get_latest_version () >>= fun t0 -> 
  (*
   * Alice invites Bob for collaboration.
   *)
  Vpst.fork_version bob_f >>= fun () ->
  (*
   * Alice replaces "fox" with "rooster".
   *)
  let t0' =  update t0 [L;R] "rooster" in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.4 >>= fun () ->
  (* Alice syncs with Bob. Gets Bob's both insertions. *)
  Vpst.sync_next_version ~v:t0' >>= fun t1 ->
  (*
   * While the fox was wheatish brown, the rooster isn't. 
   * So Alice removes "wheatish".
   *)
  let t1' = update t1 [L;L;R] "" in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Syncs with Bob again. Nothing new. *)
  Vpst.sync_next_version ~v:t1' >>= fun t2 ->
  let _ = Printf.printf "Alice: \n" in
  let _ = print_doc t2 in
  Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
  Vpst.return ()

let main () = 
  (*
   * The initial document.
   *)
  let init_doc = TreeDoc.of_list 
             ["a"; "quick"; "brown"; "fox"; "jumped";
              "over"; "a"; "lazy"; "dog"] in
   (*
    * Alice is the owner of the document, and starts the session.
    *)
   Vpst.with_init_version_do init_doc alice_f;;

main ();;
