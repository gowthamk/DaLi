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
  val to_string: t -> string
end

module type MLIST = functor(A:MERGEABLE) -> sig
  include MERGEABLE
  val empty : t Lwt.t
  val cons: A.t -> t -> t Lwt.t
  val append: t -> t -> t Lwt.t
  val print: t -> unit Lwt.t
end

module type MTREE = functor(A:MERGEABLE) -> sig
  include MERGEABLE
  val empty : t Lwt.t
  val insert: A.t -> t -> t Lwt.t
  val coalesce: t -> t -> t Lwt.t
  val print: t -> unit Lwt.t
end

module MList: MLIST = functor(A : MERGEABLE) -> struct

  type mlist = 
    | Nil 
    | Cons of A.t * K.t

  module AO_value (* : Tc.S0 *) = struct
    type t = mlist

    let equal t1 t2 = match (t1,t2) with
      | (Nil,Nil) -> true
      | (Cons (x,xs_key), Cons (y,ys_key)) -> 
          A.equal x y && K.equal xs_key ys_key 
          (* Content-addressibility means if the tail lists 
           * are equal, then their keys are equal. *)
      | _ -> false
    let compare = compare
    let hash = Hashtbl.hash

    let to_json = function
      | Nil -> `A [`String "Nil"] 
      | Cons (x,xs_key) -> `A [`String "Cons"; A.to_json x; 
                               K.to_json xs_key]
    let of_json = function
      | `A [`String "Nil"] -> Nil
      | `A [`String "Cons"; x; k] -> Cons (A.of_json x, K.of_json k)
      | j -> Ezjsonm.parse_error j "MList.AO_value.of_json"
    
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


  (* list functions *)
  let empty = 
    AO_store.create () >>= fun ao_store ->
    AO_store.add ao_store Nil

  let cons x xs_key = 
    let new_list = Cons (x,xs_key) in
      AO_store.create () >>= fun ao_store ->
      AO_store.add ao_store new_list 

  let rec append xs_key ys_key = 
    AO_store.create () >>= fun ao_store ->
    AO_store.read ao_store xs_key >>= fun xs ->
    match from_just xs with
      | Nil -> Lwt.return ys_key
      | Cons (x,xs'_key) -> 
          append xs'_key ys_key >>= fun l_key ->
          AO_store.add ao_store @@ Cons (x,l_key)

  let rec print l_key = 
    AO_store.create () >>= fun ao_store ->
    AO_store.read ao_store l_key >>= fun l ->
    match from_just l with
      | Nil -> (print_string "[]\n"; Lwt.return ())
      | Cons (x,xs_key) -> 
          begin
            Printf.printf  "%s::" @@ A.to_string x;
            print xs_key
          end

  module C (* : Irmin.Contents.S *) = struct

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
      let merge' path ~old (v1: t option) (v2: t option)  = 
        let _ = Printf.printf "In merge\n" in
          match (v1,v2) with
            | (Some xs, Some ys) -> (* this is obviously incorrect. *)
                append xs ys >>= fun l -> Lwt.return @@ `Ok (Some l)
            | (None, Some _) -> Lwt.return @@ `Ok v2
            | (Some _, None) -> Lwt.return @@ `Ok v1
            | _ -> Lwt.return @@ `Ok None in
        merge'
  end

  include C

end

(*
 * For now, we are defining a tree to be same as a list.
 *)
module MTree:MTREE = functor(A: MERGEABLE) -> struct
  module MListA = MList(A)
  include MListA
  let insert = cons
  let coalesce = append
end

module BC_store(A: MERGEABLE) = struct
  module MListA = MList(A)
  module MTreeA = MTree(A)
  type c_t = 
      | T1 of MListA.t 
      | T2 of MTreeA.t
  module C = struct
    type t = c_t

    let equal x y = match (x,y) with
      | (T1 l1, T1 l2) -> MListA.equal l1 l2
      | (T2 t1, T2 t2) -> MTreeA.equal t1 t2
      | _ -> false

    let compare = compare
    let hash = Hashtbl.hash

    let to_json = let open Ezjsonm in function
      | T1 l -> `A [`String "T1"; MListA.to_json l]
      | T2 t -> `A [`String "T2"; MTreeA.to_json t]
    let of_json = let open Ezjsonm in function
      | `A [`String "T1"; v] -> T1 (MListA.of_json v)
      | `A [`String "T2"; v] -> T2 (MTreeA.of_json v)
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
        let _ = Printf.printf "In merge\n" in
        let (old_l : unit -> MListA.t option option Irmin.Merge.result Lwt.t) = 
          fun () ->
            (old () >>= fun old -> 
             let x = match old with 
              | `Ok (Some (Some (T1 l))) -> `Ok (Some (Some l))
              | `Ok (Some None) -> `Ok (Some None) 
              | `Ok None -> `Ok None
              | `Conflict s -> `Conflict s
              | _ -> failwith "Impossible" in
              Lwt.return x) in
        let (old_t : unit -> MTreeA.t option option Irmin.Merge.result Lwt.t) = 
          fun () ->
            (old () >>= fun old -> 
             let x = match old with 
              | `Ok (Some (Some (T2 t))) -> `Ok (Some (Some t))
              | `Ok (Some None) -> `Ok (Some None) 
              | `Ok None -> `Ok None
              | `Conflict s -> `Conflict s
              | _ -> failwith "Impossible" in
              Lwt.return x) in
          match (v1,v2) with
            | (Some (T1 xs), Some (T1 ys)) -> 
                MListA.merge path old_l (Some xs) (Some ys) >>= (fun res ->
                match res with
                  | `Ok (Some l) -> Lwt.return @@ `Ok (Some (T1 l))
                  | `Ok None -> Lwt.return @@ `Ok None
                  | `Conflict s -> Lwt.return @@ `Conflict s) 
            | (Some (T2 xs), Some (T2 ys)) -> 
                MTreeA.merge path old_t (Some xs) (Some ys) >>= (fun res ->
                match res with
                  | `Ok (Some l) -> Lwt.return @@ `Ok (Some (T2 l))
                  | `Ok None -> Lwt.return @@ `Ok None
                  | `Conflict s -> Lwt.return @@ `Conflict s) 
            | (Some (T1 _), Some (T2 _)) | (Some (T2 _), Some (T1 _)) -> 
                failwith "Unexpected"
            | (None, Some _) -> Lwt.return @@ `Ok v2
            | (Some _, None) -> Lwt.return @@ `Ok v1
            | _ -> Lwt.return @@ `Ok None in
        merge'
  end

  module Store = Irmin_git.FS(C)(Irmin.Ref.String)(Irmin.Hash.SHA1)

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

module Make(A: MERGEABLE) = struct
  module BC_store = BC_store(A)
  type branch = BC_store.branch

  (*
   * Lightweight branching thread monad
   *)
  module Lwbt = struct
    type 'a t = branch -> 'a Lwt.t
    let return (x : 'a) : 'a t = 
      fun br -> Lwt.return x
    let bind (m1: 'a t) (f: 'a -> 'b t) : 'b t = 
      fun br -> (m1 br >>= fun a -> f a br)
    let master_do (f: 'a t) :'a Lwt.t =
      BC_store.init () >>= fun repo -> 
      BC_store.master repo >>= fun m -> f m
    let clone_do name (f: 'a t) : 'a t = fun br ->
      BC_store.clone_force br name >>= fun new_br ->
      f new_br
    let commit () : 'a t = fun br ->
      BC_store.init () >>= fun repo -> 
      BC_store.master repo >>= fun master -> 
      BC_store.merge br ~into:master
    let pull () : 'a t = fun br ->
      BC_store.init () >>= fun repo -> 
      BC_store.master repo >>= fun master -> 
      BC_store.merge master ~into:br
    let (>>=) = bind
  end

  module type VERSIONED = sig
    module M: MERGEABLE
    type t
    val new_of: M.t -> t Lwbt.t
    val to_m: t -> M.t
    val try_set_latest: t -> M.t -> unit Lwbt.t
    val get_latest: t -> t Lwbt.t
  end

  module VList: VERSIONED with module M = MList(A) = struct
    module M = MList(A)

    type t = path * M.t
    type branch = BC_store.branch

    (*
     * Monadic functions
     *)
    let new_of l_key = fun branch ->
      let bc_store = branch "creating a handle" in
      let new_path = let open Uuidm in [to_string @@ v `V4] in
        BC_store.update bc_store new_path (BC_store.T1 l_key) >>= fun () ->
        Lwt.return (new_path, l_key)

    let to_m (_,m) = m

    let try_set_latest (path,_) l_key branch = 
      let bc_store = branch "updating a handle" in
        BC_store.update bc_store path (BC_store.T1 l_key)

    let get_latest (path,_) branch = 
      let bc_store = branch "reading a handle" in
        BC_store.read bc_store path >>= fun c_t ->
        match from_just c_t with 
          | BC_store.T1 l_key -> Lwt.return (path, l_key)
          | _ -> failwith "Unexpected"
  end

  module VTree: VERSIONED with module M = MTree(A) = struct
    module M = MTree(A)

    type t = path * M.t
    type branch = BC_store.branch

    (*
     * Monadic functions
     *)
    let new_of t_key branch = 
      let bc_store = branch "creating a handle" in
      let new_path = let open Uuidm in [to_string @@ v `V4] in
        BC_store.update bc_store new_path (BC_store.T2 t_key) >>= fun () ->
        Lwt.return (new_path, t_key)

    let to_m (_,m) = m

    let try_set_latest (path,_) t_key branch = 
      let bc_store = branch "updating a handle" in
        BC_store.update bc_store path (BC_store.T2 t_key)

    let get_latest (path,_) branch = 
      let bc_store = branch "reading a handle" in
        BC_store.read bc_store path >>= fun c_t ->
        match from_just c_t with 
          | BC_store.T2 t_key -> Lwt.return (path, t_key)
          | _ -> failwith "Unexpected"
  end
end
