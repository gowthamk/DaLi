module Tree (A: sig
  type t
  val merge  : t -> t -> t -> t
  val concat : t -> t -> t
end) = struct

  type t =
  | Leaf of A.t
  | Node of t * int * t

  let rope_of x = failwith "Unimpl."
  let create_index t = failwith "Unimpl"
  let rec flatten = function
    | Leaf x -> x
    | Node (l,_,r) -> A.concat (flatten l) (flatten r)

  let rec merge old l r =
    if l = r then l
    else if old = l then r
    else if old = r then l
    else merge_rec old l r

  and merge_rec old l r = match (old,l,r) with
    | Leaf _, _, _ | _, Leaf _, _ | _, _, Leaf _ ->
        rope_of @@ A.merge (flatten old) (flatten l) (flatten r)
    | Node (oldl,_,oldr), Node (ll,_,lr), Node (rl,_,rr) ->
        let newl = merge oldl ll rl in
        let newr = merge oldr lr rr in
          Node (newl, create_index newl, newr)
end
