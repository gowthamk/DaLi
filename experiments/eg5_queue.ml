module List = struct
  include List
  let findi x l = 
    let i = ref None in 
    let _ = List.iteri 
              (fun j a -> 
                if a = x then i := Some j else ()) l in
      match !i with | None -> raise Not_found 
        | Some j -> j
  
  let rec sub i l = match (i,l) with
    | (0,_) -> l
    | (_,[]) -> raise @@ Invalid_argument "List.sub"
    | (_,_::xs) -> sub (i-1) xs
end

module type MYQUEUE = sig
  type 'a t = T of 'a list * 'a list
  val empty : 'a t
  val enq: 'a t -> 'a -> 'a t
  val deq: 'a t -> 'a option * 'a t
  val merge: old:'a t -> 'a t -> 'a t -> 'a t
  val print: ('a -> string) -> 'a t -> unit
end
module MyQueue:MYQUEUE = struct
  type 'a t = T of 'a list * 'a list
  let empty = T ([],[])
  let enq (T (l,r)) a = T (l,a::r)
  let rec deq = function
    | T ([],[]) -> (None, T ([],[]))
    | T (x::xs,r) -> (Some x, T (xs,r))
    | T ([],r) -> deq @@ T (List.rev r,[])

  let get_diff v v' = match (v,v') with
    | ([],v') -> (0(* no deqs *), v')
    | (_,[]) -> (List.length v(* all deqs *), [])
    | _ -> 
        let n_deqs = List.findi (List.hd v') v in
        let n_old = (List.length v) - n_deqs in 
        let enqs = List.sub n_old v' in
          (n_deqs, enqs)

  let merge ~old v1 v2 = 
    let join (T (a,b)) = a @ b in
    let (old,v1,v2) = (join old, join v1, join v2) in
    let (n_deqs1,enqs1) = get_diff old v1 in
    let (n_deqs2,enqs2) = get_diff old v2 in
    let (n_deqs,enqs) = (max n_deqs1 n_deqs2, 
                         enqs1 @ enqs2 (* Fixme: non-det order. *)) in
    let old_rest = List.sub n_deqs old in 
      T (old_rest,enqs)

  let print f (T (a,b)) = 
    let to_str l = String.concat "," @@ List.map f l in
    Printf.printf "([%s], [%s])\n" (to_str a) (to_str b)

end

open MyQueue

let main () = 
  let x1 = empty in 
  let x2 = enq (snd @@ deq (enq (enq x1 2) 1)) 3 in      
  let _ = print string_of_int x2 in 
  let x3 = enq (snd @@ deq x2) 4 in
  let _ = print string_of_int x3 in 
  let x4 = enq x2 5 in 
  let _ = print string_of_int x4 in 
  let x2' = merge x2 x3 x4 in 
  let _ = print string_of_int x2' in
    ();;

main ();;
