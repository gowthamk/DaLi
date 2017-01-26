open Lwt.Infix
open Irmin_unix
open Eg5_queue

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

module type MLIST = functor(A:Tc.S0) -> sig
  include MERGEABLE
  val of_list: A.t list -> t Lwt.t
  val to_list: t -> A.t list Lwt.t
end

module type MQUEUE = functor(A:Tc.S0) -> sig
  include MERGEABLE
  val of_queue : (A.t list * A.t list) -> t Lwt.t
  val to_queue: t -> (A.t list * A.t list) Lwt.t
end

module MList: MLIST = functor(A : Tc.S0) -> struct

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

  let cons x xs = 
    let new_list = Cons (x,xs) in
      AO_store.create () >>= fun ao_store ->
      AO_store.add ao_store new_list 

  let rec of_list = function 
    | [] -> empty
    | x::xs -> of_list xs >>= fun xs_key -> cons x xs_key 

  let rec to_list l_key = 
    AO_store.create () >>= fun ao_store ->
    AO_store.read ao_store l_key >>= fun l ->
    match from_just l with
      | Nil -> Lwt.return []
      | Cons (x,xs_key) -> 
          to_list xs_key >>= fun xs -> Lwt.return @@ x::xs


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
    failwith ""

end

module MQueue: MQUEUE = functor(A : Tc.S0) -> struct

  module M = MList(A)
  type mqueue = T of M.t * M.t

  module AO_value (* : Tc.S0 *) = struct
    type t = mqueue

    let equal t1 t2 = match (t1,t2) with
      | (T (a1,b1), T (a2,b2)) -> M.equal a1 a2 && M.equal b1 b2
    let compare = compare
    let hash = Hashtbl.hash

    let to_json = function 
      | T (a,b) -> `A [`String "T"; M.to_json a; M.to_json b]
    let of_json = function
      | `A [`String "T"; a; b] -> T (M.of_json a, M.of_json b)
      | j -> Ezjsonm.parse_error j "MQueue.AO_value.of_json"
    
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

  let of_queue (a,b) = 
    M.of_list a >>= fun x ->
    M.of_list b >>= fun y ->
    AO_store.create () >>= fun ao_store ->
    AO_store.add ao_store (T (x,y))

  let rec to_queue q_key = 
    AO_store.create () >>= fun ao_store ->
    AO_store.read ao_store q_key >>= fun q ->
    match from_just q with
      | T (x,y) -> 
          M.to_list x >>= fun a -> 
          M.to_list y >>= fun b ->
          Lwt.return @@ (a,b)

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
        let _ = Printf.printf "In merge\n" in
        let old = match Lwt_main.run @@ old () with
          | `Ok (Some x) -> x | _ -> failwith "Impossible" in
        let f x = Lwt_main.run 
                    (to_queue x >>= fun y ->
                     match y with 
                       | (l1,l2) -> Lwt.return @@ MyQueue.T (l1,l2)) in
        let g (MyQueue.T (x,y)) = Some (Lwt_main.run @@ of_queue (x,y)) in
          match (old,v1,v2) with
            | (Some old, Some v1, Some v2) -> 
                Lwt.return @@ `Ok (g @@ MyQueue.merge (f old) (f v1) (f v2))
            | _ -> failwith "Impossible" in
      merge'

end
