module Tree (C: sig
  type t
  val merge  : t -> t -> t -> t
  val concat : t -> t -> t
end) = struct

  type t =
  | Leaf of C.t
  | Node of t * t

  let make_tree (v : C.t) : t = failwith "undefined"

  let rec linearize = function
  | Leaf v -> v
  | Node (l,r) -> C.concat (linearize l) (linearize r)

  let merge_linear old l r =
    C.merge (linearize old) (linearize l) (linearize r)

  let rec merge old l r =
    if l = r then l
    else if old = l then r
    else if old = r then l
    else merge_rec old l r

  and merge_rec old l r =
    match (l,r) with
    | Leaf _, _
    | _, Leaf _ -> make_tree (merge_linear old l r)
    | Node (ll,lr), Node (rl,rr) ->
        match old with
        | Leaf _ -> make_tree (merge_linear old l r)
        | Node (oldl, oldr) ->
            let newl = merge oldl ll rl in
            let newr = merge oldr lr rr in
            Node (newl, newr)

end
