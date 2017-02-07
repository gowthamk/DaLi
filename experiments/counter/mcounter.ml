open Lwt.Infix
open Irmin_unix

module G = Git_unix.FS
module K = Irmin.Hash.SHA1
module M = Irmin.Merge

type path = string list

let from_just = function (Some x) -> x
  | None -> failwith "Expected Some. Got None."

module type MERGEABLE = sig
  include Irmin.Contents.S with module Path = Irmin.Path.String_list
end

module type MCOUNTER = sig
  include MERGEABLE with type t = int
end

module MCounter: MCOUNTER = struct
  include Tc.Int
  (*
   * Counter ops
   *) 
  let inc = Counter.inc
  let dec = Counter.dec
  let merge = Counter.merge

  module Path = Irmin.Path.String_list

  (*
   * A wrapper for merge that has the type required
   * by Irmin.
   *)
  let merge : Path.t -> t option Irmin.Merge.t = 
    fun path ~old (v1: t option) (v2: t option) 
          : t option Irmin.Merge.result Lwt.t ->
      let old = match Lwt_main.run @@ old () with
        | `Ok (Some x) -> x 
        | _ -> failwith "Impossible" (* ToDo: Dali merges 
               always have a common ancestor. *) in
      let c = merge (from_just old) (from_just v1) 
          (from_just v2) in
        Lwt.return @@ `Ok (Some c)
end

module BC_store = struct
  module Path = Irmin.Path.String_list
  module Store = Irmin_git.FS(MCounter)(Irmin.Ref.String)(Irmin.Hash.SHA1)
  type repo = Store.Repo.t
  type branch = string -> Store.t

  type path = string list

  let init ?root ?bare () =
    let config = Irmin_git.config ?root ?bare () in
    Store.Repo.create config

  let master (repo:repo) : branch Lwt.t = Store.master task repo
  let clone_force t name = Store.clone_force task (t "cloning") name
  let get_branch r ~branch_name = Store.of_branch_id task branch_name r
  let merge b ~into = Store.merge_exn "" b ~into
  let get_branch_name b = Store.name (b "name")
  let update = Store.update
  let read = Store.read
end

module Vpst : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val with_init_version_do: Counter.t -> 'a t -> 'a
  val fork_version : 'a t -> unit t
  val get_latest_version: unit -> Counter.t t
  val sync_next_version: ?v:Counter.t -> unit -> Counter.t t
  val liftLwt : 'a Lwt.t -> 'a t
end= struct
  type branch = BC_store.branch
  type st = {master    : branch;
             local     : branch;
             name      : string;
             next_id   : int}
  type 'a t = st -> ('a * st) Lwt.t

  (* The path at which the db is stored on all branches. *)
  let path = ["state"]

  let return (x : 'a) : 'a t = 
    fun st -> Lwt.return (x,st)

  let bind (m1: 'a t) (f: 'a -> 'b t) : 'b t = 
    fun st -> (m1 st >>= fun (a,st') -> f a st')

  let with_init_version_do (v: Counter.t) (m: 'a t) =
    Lwt_main.run 
      begin
        BC_store.init () >>= fun repo -> 
        BC_store.master repo >>= fun m_br -> 
        let m_store = m_br "creating state on master" in
        BC_store.update m_store path v >>= fun () ->
        BC_store.clone_force m_br "1_local" >>= fun t_br ->
        let st = {master=m_br; local=t_br; name="1"; next_id=1} in
        m st >>= fun (a,_) -> Lwt.return a
      end
    
  let fork_version (m: 'a t) :unit t = fun (st: st) ->
    let thread_f () = 
      let child_name = st.name^"_"^(string_of_int st.next_id) in
      let parent_m_br = st.master in
      (* Ideally, the following has to happen: *)
      (* BC_store.clone_force parent_m_br m_name >>= fun m_br -> *)
      (* But, we currently default to an SC mode. Master is global. *)
      let m_br = parent_m_br in
      BC_store.clone_force m_br (child_name^"_local") >>= fun t_br ->
      let new_st = {master = m_br; local  = t_br; 
                    name = child_name; next_id = 1} in
        m new_st in
    begin
      Lwt.async thread_f;
      Lwt.return ((), {st with next_id=st.next_id+1})
    end

  let get_latest_version () : Counter.t t = fun (st: st) ->
    let bc_store = st.local "reading local state" in
    BC_store.read bc_store path >>= fun v ->
    Lwt.return (from_just v,st)

  let sync_next_version ?v () : Counter.t t = fun (st: st) ->
    (* How do you commit the next version? Simply update path? *)
    (* 1. Commit to the local branch *)
    let bc_store = st.local "committing local state" in
    (match v with | None -> Lwt.return ()
      | Some v -> BC_store.update bc_store path v) >>= fun () ->
    (* 2. Merge local master to the local branch *)
    BC_store.merge st.master ~into:st.local >>= fun () ->
    (* 3. Merge local branch to the local master *)
    BC_store.merge st.local ~into:st.master >>= fun () ->
    get_latest_version () st
   
  let liftLwt (m: 'a Lwt.t) : 'a t = fun st ->
    m >>= fun a -> Lwt.return (a,st)

end

