open Lwt.Infix
open Irmin_unix
open Treedoc

module G = Git_unix.FS
module K = Irmin.Hash.SHA1
module M = Irmin.Merge

type path = string list

let from_just = function (Some x) -> x
  | None -> failwith "Expected Some. Got None."

module type MERGEABLE = sig
  include Irmin.Contents.S with module Path = Irmin.Path.String_list
  val to_string: t -> string
end

module type MTREEDOC = functor(A:Tc.S0) -> sig
  include MERGEABLE
  val of_treedoc: A.t TreeDoc.t -> t Lwt.t
  val to_treedoc: t -> A.t TreeDoc.t Lwt.t
end

module MTreeDoc: MTREEDOC = functor(A:Tc.S0) -> struct
  type mtreedoc = 
    | N
    | B of K.t * A.t * K.t

  module AO_value (* : Tc.S0 *) = struct
    type t = mtreedoc

    let equal t1 t2 = match (t1,t2) with
      | (N,N) -> true
      | (B (lt_key1, x1, rt_key1), B (lt_key2, x2, rt_key2)) -> 
          A.equal x1 x2 && K.equal lt_key1 lt_key2 && 
                           K.equal rt_key1 rt_key2 
      | _ -> false
    let compare = compare
    let hash = Hashtbl.hash

    let to_json = function
      | N -> `A [`String "N"] 
      | B (lt_key,x,rt_key) -> `A [`String "B"; 
                                      K.to_json lt_key; 
                                      A.to_json x; 
                                      K.to_json rt_key]
    let of_json = function
      | `A [`String "N"] -> N
      | `A [`String "B"; k1; x; k2] -> 
          B (K.of_json k1, A.of_json x, K.of_json k2)
      | j -> Ezjsonm.parse_error j "MTreeDoc.AO_value.of_json"
    
    let to_string t = Ezjsonm.to_string (to_json t)
    let of_string s = of_json (Ezjsonm.from_string s)
    let write t buf =
      let str = to_string t in
      let len = String.length str in
      Cstruct.blit_from_string str 0 buf 0 len;
      Cstruct.shift buf len
    let read buf =
      Mstruct.get_string buf (Mstruct.length buf)
      |> of_string
    let size_of t =
      let str = to_string t in
      String.length str
  end

  module AO_store = struct
    module S = Irmin_git.AO(G)(K)(AO_value)
    include S

    let create config =
      let level = Irmin.Private.Conf.key ~doc:"The Zlib compression level."
        "level" Irmin.Private.Conf.(some int) None
      in
      let root = Irmin.Private.Conf.get config Irmin.Private.Conf.root in
      let level = Irmin.Private.Conf.get config level in
      G.create ?root ?level ()

    (* Somehow pulls the config set by Store.init *)
    (* And creates a Git backend *)
    let create () = create @@ Irmin_git.config ()
  end

  (* treedoc functions *)
  let empty = 
    AO_store.create () >>= fun ao_store ->
    AO_store.add ao_store N

  let cons lt x rt = 
    let new_tree = B (lt,x,rt) in
      AO_store.create () >>= fun ao_store ->
      AO_store.add ao_store new_tree

  let rec of_treedoc = function 
    | TreeDoc.N -> empty
    | TreeDoc.B (lt,x,rt) -> 
        of_treedoc lt >>= fun lt_key -> 
        of_treedoc rt >>= fun rt_key -> 
        cons lt_key x rt_key 

  let rec to_treedoc t_key = 
    AO_store.create () >>= fun ao_store ->
    AO_store.read ao_store t_key >>= fun t ->
    match from_just t with
      | N -> Lwt.return TreeDoc.N
      | B (lt_key,x,rt_key) -> 
          to_treedoc lt_key >>= fun lt -> 
          to_treedoc rt_key >>= fun rt ->
          Lwt.return @@ TreeDoc.B (lt,x,rt)

  exception MergeConflict
  let rec merge old_k v1_k v2_k : K.t Lwt.t = 
    if v1_k=v2_k then Lwt.return v1_k
    else if v1_k=old_k then Lwt.return v2_k
    else if v2_k=old_k then Lwt.return v1_k
    else 
      AO_store.create () >>= fun ao_store ->
      AO_store.read ao_store old_k >>= fun old ->
      AO_store.read ao_store v1_k >>= fun v1 ->
      AO_store.read ao_store v2_k >>= fun v2 ->
      match (from_just old, from_just v1, from_just v2) with
        | (N, B _, N) -> Lwt.return v1_k (* new sub-tree in v1 *)
        | (N, N, B _) -> Lwt.return v2_k (* new sub-tree in v2 *)
        | (N, B (lt1_k,x1,rt1_k), B (lt2_k,x2,rt2_k)) -> 
            (* new sub-trees in v1 and v2 *)
            if x1=x2 (* and they are compatible *)
            then 
              empty >>= fun e ->
              merge e lt1_k lt2_k >>= fun lt_k ->
              merge e rt1_k rt2_k >>= fun rt_k ->
              cons lt_k x1 rt_k
            else raise MergeConflict (* not compatible *)
        | (B (lt_k,x,rt_k), B (lt1_k,x1,rt1_k), B (lt2_k,x2,rt2_k)) ->
            let lt'() = merge lt_k lt1_k lt2_k in
            let rt'() = merge rt_k rt1_k rt2_k in
            let ret x = lt'() >>= fun lt' -> rt'() >>= fun rt' ->
              cons lt' x rt' in
              if (x1=x2) then ret x1
              else if (x1=x) then ret x2
              else if (x2=x) then ret x1
              else raise MergeConflict (* same text edited *)
        | (B _,N, _) | (B _, _, N) -> 
            failwith "Impossible; with tombstones, branch 
                      cannot be deleted."
        | (N,N,N) -> empty (* for the sake of completeness *)
    

  type t = K.t

  let equal t1 t2 = true
  let compare = compare
  let hash = Hashtbl.hash

  let to_json k = `A [`String (K.to_hum k)]
  let of_json = function
    | `A [`String kstr] -> K.of_hum kstr
    | j -> Ezjsonm.parse_error j "MList_contents.C.of_json"
  
  let to_string t = Ezjsonm.to_string (to_json t)
  let of_string s = of_json (Ezjsonm.from_string s)
  let write t buf =
    let str = to_string t in
    let len = String.length str in
    Cstruct.blit_from_string str 0 buf 0 len;
    Cstruct.shift buf len
  let read buf =
    Mstruct.get_string buf (Mstruct.length buf)
    |> of_string
  let size_of t =
    let str = to_string t in
    String.length str

  module Path = Irmin.Path.String_list

  let merge : Path.t -> t option Irmin.Merge.t = 
    let merge' path ~old (v1: t option) (v2: t option)
          : t option Irmin.Merge.result Lwt.t =
        let old = match Lwt_main.run @@ old () with
          | `Ok (Some x) -> x 
          | _ -> failwith "Impossible" (* ToDo: Dali merges 
                 always have a common ancestor. Change types
                 to reflect this guarantee *) in
          try 
            merge (from_just old) (from_just v1) 
              (from_just v2) >>= fun k ->
            Lwt.return @@ `Ok (Some k)
          with MergeConflict -> 
            Lwt.return @@ `Conflict "TreeDoc: Incompatible 
                            concurrent versions"
    in
      merge'
end

module BC_store(A: MERGEABLE) = struct
  module MA = MTreeDoc(A)
  module Path = Irmin.Path.String_list
  module Store = Irmin_git.FS(MA)(Irmin.Ref.String)(Irmin.Hash.SHA1)
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

module Vpst(A: MERGEABLE): sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val with_init_version_do: A.t TreeDoc.t -> 'a t -> 'a
  val fork_version : 'a t -> unit t
  val get_latest_version: unit -> A.t TreeDoc.t t
  val sync_next_version: ?v:A.t TreeDoc.t -> A.t TreeDoc.t t
  val liftLwt : 'a Lwt.t -> 'a t
end = struct
  module MA = MTreeDoc(A)
  module BC_store = BC_store(A)
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

  let with_init_version_do (v: A.t TreeDoc.t) (m: 'a t) =
    Lwt_main.run 
      begin
        BC_store.init () >>= fun repo -> 
        BC_store.master repo >>= fun m_br -> 
        let m_store = m_br "creating state on master" in
        MA.of_treedoc v >>= fun k ->
        BC_store.update m_store path k >>= fun () ->
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

  let get_latest_version () : A.t TreeDoc.t t = fun (st: st) ->
    let bc_store = st.local "reading local state" in
    BC_store.read bc_store path >>= fun k ->
    MA.to_treedoc @@ from_just k >>= fun td ->
    Lwt.return (td,st)

  let sync_next_version ?v : A.t TreeDoc.t t = fun (st: st) ->
    (* How do you commit the next version? Simply update path? *)
    (* 1. Commit to the local branch *)
    let bc_store = st.local "committing local state" in
    (match v with | None -> Lwt.return ()
      | Some v -> 
          MA.of_treedoc v >>= fun k -> 
          BC_store.update bc_store path k) >>= fun () ->
    (* 2. Merge local master to the local branch *)
    BC_store.merge st.master ~into:st.local >>= fun () ->
    (* 3. Merge local branch to the local master *)
    BC_store.merge st.local ~into:st.master >>= fun () ->
    get_latest_version () st
   
  let liftLwt (m: 'a Lwt.t) : 'a t = fun st ->
    m >>= fun a -> Lwt.return (a,st)

end

