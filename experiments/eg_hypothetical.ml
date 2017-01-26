(*
 * Hypothetical example to demonstrate programming model.
 *)

(*
 * The Versioned Persistent State Monad
 * 1. Lets you create concurrent versions of the given state.
 * 2. Lets you persist the state, and resume the computation using a 
 *    previously persisted state.
 *)

module type VPST = sig
  module Vpst_snapshot: Tc.S1
  type 'a 'b t (* the key is the parameterization over any 'a *)
  val with_init_version_do: 'a -> 'a 'b t -> 'b
  val with_snapshot_do: 'a Vpst_snapshot.t -> 'a 'b t -> 'b
  val fork_versions: 'a 'b t list -> 'a 'b t
  val get_latest_version: unit -> 'a 'a t
  val set_later_version: 'a -> 'a unit t
  val local_snapshot: unit -> 'a ('a Vpst_snapshot.t) t
end

(*
 * the type of the state we want to maintain in the database.
 *)
type 'a db = {q: 'a Queue.t; timestamp: Time.t}

(*
 * The main function. Please read from the bottom.
 *)
               (int db) ((int db) Vpst_snapshot.t) Vpst.t
               (int db) (int) Vpst.t
let main () = 
  let (>>=) a b = let open Vpst in a >>= b in
  (*
   * Each thread has its own (persistent) local copy of the state. 
   * The thread adds its id to the queue and updates the time stamp.
   * If the thread is created by the top_level, then it forks two
   * more threads which repeat the same. Otherwise exits.
   *)
  let thread_f i  = 
    (* Get the db object behind the version wrapper. *)
    (* get_latest_version: unit -> 'a 'a Vpst.t *)
    Vpst.get_latest_version () >>= fun (db: int db) ->
    Vpst.thread_id () >>= fun j ->
    (* Make new db object *)
    let db' = {db with q = Queue.enq db.q j;
                       timestamp = Time.get_unixtime()} in
    (* Mark db' as a later version of db. If other concurrent 
     * threads also did the same, then merge concurrent versions *)
    (* set_later_version: 'a -> 'a unit Vpst.t *)
    Vpst.set_later_version db' >>= fun () ->
    (* Get the latest version post the last merge *)
    Vpst.get_latest_version () >>= fun db' ->
    (* If this is a second-level thread, then return *)
    if i=2 then (* Vpst.snapshot () >>= fun s -> Vpst.return s *) Vpst.return 0
    else begin
          (* Else, fork 2 new second-level threads from the current local
           * (persistent) state*)
           Vpst.fork_versions vdb' [thread_f 2; thread_f 2] >>= fun _ ->
          (* After the threads return, get the lastest version of db
           * and return *)
           Vpst.local_snapshot () >>= fun s -> Vpst.return s
         end in
  (*
   * Top-level function. Receives a versioned db (vdb), and forks two
   * new threads. Each thread gets a local copy of vdb.
   * -- Vpst is a branching versioned state monad.
   *)
  let top_level_f  =
    (* 'a 'b Vpst.t list -> 'a 'b Vpst.t *) (* 'a is the type of the state. 'b is
    the type of the return value *)
    Vpst.fork_versions (* vdb *) [thread_f 0; thread_f 0] in
  (* The initial db *)
  let init_db = {q = Queue.empty; timestamp= Time.get_unixtime()} in
  (*
   * With versioned init_db as the initial state, run the top_level
   * computation.
   *)
  (* (Serializable 'a, Versioned 'b) => 'a -> 'a 'b Vpst.t -> 'b *)
  let s = Vpst.with_init_version_do init_db top_level_f in
  let _ = File.put_str (File.open_file "/var/db_backup") @@ 
              Vpst_snapshot.to_json s in
    ()

