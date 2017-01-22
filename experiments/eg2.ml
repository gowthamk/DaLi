open Lwt.Infix
open Irmin_unix

let from_just = function (Some x) -> x
  | None -> failwith "Expected Some. Got None."

module MList(A : sig 
               include Tc.S0
               val to_string : t -> string
             end) = struct

  module G = Git_unix.FS
  module K = Irmin.Hash.SHA1

  type path = string list
  type t = K.t
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

  module BC_store = struct
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

  (*
   * Monadic functions
   *)
  let add branch l_key = 
    let bc_store = branch "creating a handle" in
    let new_path = let open Uuidm in [to_string @@ v `V4] in
      BC_store.update bc_store new_path l_key >>= fun () ->
      Lwt.return new_path

  let update branch path l_key = 
    let bc_store = branch "updating a handle" in
      BC_store.update bc_store path l_key

  let set branch path l_key = 
    let bc_store = branch "updating a handle" in
      BC_store.update bc_store path l_key 

  let get branch path = 
    let bc_store = branch "reading a handle" in
      BC_store.read bc_store path 

end


module IntMList = MList(struct
                          include Tc.Int
                          let to_string = string_of_int
                        end)
module BC_store = IntMList.BC_store

let (run : 'a Lwt.t -> 'a) = Lwt_main.run 

let main () =
  let master = run (BC_store.init () >>= fun repo -> 
                    BC_store.master repo) in
  let wip = run @@ BC_store.clone_force master "wip" in
  let (x: BC_store.path Lwt.t) = 
    IntMList.empty >>= fun l1 -> 
    IntMList.cons 1 l1 >>= fun l2 ->
    IntMList.cons 2 l2 >>= fun l3 ->
    IntMList.add master l3 >>= fun path ->
    IntMList.get master path >>= fun l_key ->
    IntMList.print (from_just l_key) >>= fun () -> 
    Lwt.return path in
  let path = run x in
  let (y: unit Lwt.t) =
    IntMList.empty >>= fun l1 -> 
    IntMList.cons 4 l1 >>= fun l2 ->
    IntMList.update wip path l2 >>= fun () ->
    IntMList.get wip path >>= fun l_key ->
    IntMList.print (from_just l_key) in
  let _ = run y in
  let _ = run @@ BC_store.merge wip ~into:master in
  let z = 
    IntMList.get master path >>= fun l_key ->
    IntMList.print (from_just l_key) in
    run z;;

main ();;
