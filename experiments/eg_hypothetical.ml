(*
 * Hypothetical example to demonstrate programming model.
 *)

(*
 * the type of the state we want to maintain in the database.
 *)
type 'a db = {q: 'a Queue.t; timestamp: Time.t}

(*
 * The main function. Please read from the bottom.
 *)
let main () = 
  let (>>=) a b = let open Lwb in a >>= b in
  (*
   * Each thread has its own (persistent) local copy of the state. 
   * The thread adds it's id to the queue and updates the time stamp.
   * If the thread is created by the top_level, then it forks two
   * more threads which repeat the same. Otherwise exits.
   *)
  let thread_f i (vdb : int db Versioned.t) = 
    (* Get the db object behind the version wrapper. *)
    Versioned.to_o vdb >>= fun db ->
    Lwb.thread_id () >>= fun j ->
    (* Make new db object *)
    let db' = {db with q = Queue.enq db.q j;
                       timestamp = Time.get_unixtime()} in
    (* Mark db' as a later version of db. If other concurrent 
     * threads also did the same, then merge concurrent versions *)
    Versioned.later_version vdb db' >>= fun () ->
    (* Get the latest version post the last merge *)
    Versioned.latest_version vdb >>= fun vdb' ->
    (* If this is a second-level thread, then return *)
    if i=2 then Lwb.return vdb' 
    else begin
          (* Else, fork 2 new second-level threads from the current local
           * (persistent) state*)
           Lwb.fork_versions vdb' [thread_f 2; thread_f 2] >>= fun _ ->
          (* After the threads return, get the lastest version of db
           * and return *)
           Versioned.latest_version vdb >>= fun vdb'' ->
           Lwb.return vdb''
         end in
  (*
   * Top-level function. Receives a versioned db (vdb), and forks two
   * new threads. Each thread gets a local copy of vdb.
   *)
  let top_level_f (vdb : int db Versioned.t) =
    Lwb.fork_versions vdb [thread_f 0; thread_f 0] in
  (* The initial db *)
  let init_db = {q = Queue.empty; timestamp= Time.get_unixtime()} in
  (*
   * With versioned init_db as the initial state, run the top_level
   * computation.
   *)
  let final_db = Lwb.with_init_version_do init_db top_level_f in
    final_db

