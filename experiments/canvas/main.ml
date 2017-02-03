open Icanvas
open Canvas

let (>>=) = Vpst.bind

let mk t = {max_x=128; max_y=128; t=t}

let bob_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun t0 -> 
  let loc = {x=98;y=17} in
  let c0 = mk t0 in
  let c0' = set_px c0 loc @@ rgb @@ Char.chr 23 in
  Vpst.sync_next_version ~v:c0'.t >>= fun t1 ->
  let loc = {x=45; y=78} in
  let c1 = mk t1 in
  let c1' = set_px c1 loc @@ rgb @@ Char.chr 111 in
  Vpst.liftLwt @@ Lwt_unix.sleep 1.0 >>= fun () ->
  Vpst.sync_next_version ~v:c1'.t >>= fun t2 ->
  let _ = Printf.printf "Bob: \n" in
  let _ = Canvas.print @@ mk t2 in
  Vpst.return ()
  
let alice_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun t0 -> 
  (*
   * Alice invites Bob for collaboration.
   *)
  Vpst.fork_version bob_f >>= fun () ->
  let loc = {x=93;y=127} in
  let c0 = mk t0 in
  let c0' = set_px c0 loc @@ rgb @@ Char.chr 23 in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.4 >>= fun () ->
  Vpst.sync_next_version ~v:c0'.t >>= fun t1 ->
  let loc = {x=45; y=78} in
  let c1 = mk t1 in
  let c1' = set_px c1 loc @@ rgb @@ Char.chr 17 in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  Vpst.sync_next_version ~v:c1'.t >>= fun t2 ->
  let _ = Printf.printf "Alice: \n" in
  let _ = Canvas.print @@ mk t2 in
  Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
  Vpst.return ()

let main () =
  let (f: Canvas.t -> unit Vpst.t -> unit) = Vpst.with_init_version_do in
   f blank alice_f;;

main ();;
