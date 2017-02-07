open Mcounter
open Counter

let (>>=) = Vpst.bind

let thread2_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun c0 -> 
  (* Thread2 increments the counter once by 10  *)
  let c0' = Counter.inc c0 10 in
  (* Thread2 syncs with master. Observes no changes. *)
  Vpst.sync_next_version ~v:c0' () >>= fun c1 ->
  (* Thread2 blocks for 0.5s *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.5 >>= fun () ->
  (* Thread2 decrements by 9. *)
  let c1' = Counter.dec c1 9 in 
  (* 
   * Syncs with master. Merges master's counter
   * value (11) with the local value (1). The common 
   * ancestor is 10. The final value is 2.
   *)
  Vpst.sync_next_version ~v:c1' () >>= fun c2 ->
  let _ = Printf.printf "thread2 (before exiting): %d\n" c2 in
  Vpst.return ()
  
let thread1_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun c0 -> 
  (* Thread1 forks thread2 *)
  Vpst.fork_version thread2_f >>= fun () ->
  (* Thread1 blocks for 0.1s *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Increments the counter twice - by 2 and 3, resp. *)
  let c0' = Counter.inc (Counter.inc c0 2) 3 in
  (* Syncs with master. Merges the current local value 
   * (5) with master's value (10). The common ancestor
   * is 0. The result 15. *)
  Vpst.sync_next_version ~v:c0' () >>= fun c1 ->
  (* Thread1 blocks on some operation *)
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* Decrements by 4. *)
  let c1' = Counter.dec c1 4 in
  (* Syncs with the master again. Publishes 11. *)
  Vpst.sync_next_version ~v:c1' () >>= fun c2 ->
  let _ = Printf.printf "thread1: %d\n" c2 in
  Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
  Vpst.sync_next_version () >>= fun c3->
  let _ = Printf.printf "thread1 (before exiting): %d\n" c3 in
  Vpst.return ()

let main () =
   (*
    * thread1 starts with a blank canvas.
    *)
   Vpst.with_init_version_do 0 thread1_f;;

main ();;
