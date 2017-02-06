open Icanvas
open Canvas

let (>>=) = Vpst.bind

let mk t = {max_x=128; max_y=128; t=t}

let bob_f : unit Vpst.t = 
  Vpst.get_latest_version () >>= fun t0 -> 
  let loc = {x=98;y=17} in
  let c0 = mk t0 in
  (* 
   * Bob sets the rgb value of the pixel at (98,17) to
   * (23,23,23).
   *)
  let c0' = set_px c0 loc @@ rgb @@ Char.chr 23 in
  (* Bob syncs with Alice. Observes no changes. *)
  Vpst.sync_next_version ~v:c0'.t >>= fun t1 ->
  let loc = {x=45; y=78} in
  let c1 = mk t1 in
  (*
   * Bob now colors (45,78) pixel with an rgb value of
   * (111,111,111).
   *)
  let c1' = set_px c1 loc @@ rgb @@ Char.chr 111 in
  Vpst.liftLwt @@ Lwt_unix.sleep 1.0 >>= fun () ->
  (* 
   * Bob syncs with Alice. Gets latest updates. 
   * Since Alice has also colored (45,78) pixel, but with an 
   * rgb value of (17,17,17), the resultant color of the pixel 
   * is the overlap of both colors.
   *)
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
  (* 
   * Alice sets the rgb value of the pixel at (93,127) to
   * (23,23,23).
   *)
  let c0' = set_px c0 loc @@ rgb @@ Char.chr 23 in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.4 >>= fun () ->
  (* Alice syncs with Bob. Sees Bob's coloring at (98,17) pixel. *)
  Vpst.sync_next_version ~v:c0'.t >>= fun t1 ->
  let loc = {x=45; y=78} in
  let c1 = mk t1 in
  (*
   * Alice now sets the color of (45,78) pixel to 17.
   *)
  let c1' = set_px c1 loc @@ rgb @@ Char.chr 17 in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  (* 
   * Alice syncs with Bob. Gets latest updates. 
   * Since Bob has also colored (45,78) pixel, the resultant
   * color of the pixel is the overlap of Alice's and Bob's.
   *)
  Vpst.sync_next_version ~v:c1'.t >>= fun t2 ->
  let _ = Printf.printf "Alice: \n" in
  let _ = Canvas.print @@ mk t2 in
  Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
  Vpst.return ()

let main () =
  let (f: Canvas.t -> unit Vpst.t -> unit) = Vpst.with_init_version_do in
   (*
    * Alice starts with a blank canvas.
    *)
   f blank alice_f;;

main ();;
