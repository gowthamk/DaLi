module MList = functor(A:sig
                         type t
                         val edit_seq: t -> t -> 'a list option
                         val merge: t -> t -> t -> t
                       end) -> struct

  type edit = I of A.t * int
    | D of int
    | S of int * A.t * A.t
    | Nop

 (*
  * Assumption: subst edits are at the end of the edit seq.
  *)
  let hoist_substs edits = 
    let (substs,others) = List.partition 
                            (function | S (_,_,_) -> true
                               | _ -> false) edits in
    let xform other subst = match (other, subst)  with 
      | (I (_,i), S (j,x,y)) when (j>i) -> S (j-1,x,y)
      | (D i, S (j,x,y)) when (j>=i) ->  S (j+1,x,y)
      | _ -> subst in
    let substs' = List.map 
                    (List.fold_right xform others) substs in
      substs'@others

  let op_transform mine others = 
    let xform my other = match (my,other)  with 
      | (I (x,j), I (_,i)) when (j>=i) -> I (x,j+1)
      | (D j, I (_,i)) when (j>=i) -> D (j+1)
      | (S (j,x,y), I (_,i)) when (j>=i) -> S (j+1,x,y)
      | (I (x,j), D i) when (j>i) ->  I (x,j-1)
      | (D j, D i) when (j=i) ->  Nop
      | (D j, D i) when (j>i) ->  D (j-1)
      | (S (j,x,y), D i) when (j=i) -> Nop
      | (S (j,x,y), D i) when (j>i) -> S (j-1,x,y)
      | (S (j,x,y), S (i,_,z)) when (j=i) -> S (j,x,A.merge x y z) 
      | _ -> my in
    let mine' = List.map (fun my -> List.fold_left 
                                      xform my others) @@ 
                hoist_substs mine in
      mine'

end

module MChar = struct
  type t = char
  let edit_seq x y = Some []
  let merge lca v1 v2 = 
    let _ = Printf.printf "MChar.merge\n" in
      match (lca,v1,v2) with
      | (_,'d','s') -> 'f'
      | (_,'s','d') -> 'f'
      | _ -> failwith "Other merge!"
end

module M = MList(MChar)
open M

let string_of_edit = function 
  | I (c,i) -> Printf.sprintf "I (%c,%d)" c i
  | D i -> Printf.sprintf "D (%d)" i
  | S (i,c,c') -> Printf.sprintf "S (%d,%c,%c)" i c c'
  | Nop -> ""

let string_of_edits edits = 
  String.concat "," @@ List.map string_of_edit edits

let main () = 
  let lca = ['a';'b';'c'] in
  let p = [I('c',0); S (3,'c','s')] in
  let q = [D 1; S (1,'c','d')] in
  let q' = hoist_substs q in
  let p' = hoist_substs p in 
  let q_star = op_transform q p in
  let p_star = op_transform p q in
    begin
      Printf.printf "P': %s\n" @@ string_of_edits p';
      Printf.printf "Q': %s\n" @@ string_of_edits q';
      Printf.printf "Q*: %s\n" @@ string_of_edits q_star;
      Printf.printf "P*: %s\n" @@ string_of_edits p_star;
    end;;

main();;
