module TreeDoc = struct

  type 'a t = 
    | N 
    | B of 'a t * 'a * 'a t

  type dir = L | R (* You can either take left or right *)
  type path = dir list

  let rec insert_left t x = match t with
    | N -> B (N, x, N)
    | B (t1,_,t2) -> insert_left t1 x

  let rec insert t path x = match (path,t) with
    | ([], N) -> B (N, x, N)
    | (L::path', B (lt,y,rt)) -> 
        let lt' = insert lt path' x in B (lt',y,rt)
    | (R::path', B (lt,y,rt)) -> 
        let rt' = insert rt path' x in B (lt,y,rt')
    | _ -> failwith "Unexpected path for insertion"

  let rec update t path x = match (path,t) with
    | ([], B (lt,y,rt)) -> B (lt,x,rt)
    | (L::path', B (lt,y,rt)) -> 
        let lt' = update lt path' x in B (lt',y,rt)
    | (R::path', B (lt,y,rt)) -> 
        let rt' = update rt path' x in B (lt,y,rt')
    | _ -> failwith "Unexpcted path for modification"

  let rec in_order_iter f = function
    | N -> ()
    | B (lt,y,rt) -> (in_order_iter f lt; f y; in_order_iter f rt)

  let rec of_array arr low high = 
    if low > high then N
    else 
      (* let _ = Printf.printf "low:%d, high:%d\n" low high in *)
      let mid = low + ((high - low + 1) / 2) in
      let lt = of_array arr low (mid-1) in
      let rt = of_array arr (mid+1) high in
        B (lt, arr.(mid), rt)

  let of_list = function
    | [] -> N
    | l -> of_array (Array.of_list l) 0 @@ (List.length l) - 1

  let print_doc t = 
    begin
      in_order_iter (fun s -> Printf.printf "%s " s) t;
      Printf.printf "\n"
    end

  (*
   * Merge assumes only updatations; no deletions. A deletion
   * should be modeled as an updatation.
   *)
  exception MergeConflict
  let rec merge old v1 v2 = 
    if v1=v2 then v1
    else if v1=old then v2
    else if v2=old then v1
    else match (old,v1,v2) with
      | (N, B _, N) -> v1 (* new sub-tree in v1 *)
      | (N, N, B _) -> v2 (* new sub-tree in v2 *)
      | (N, B (lt1,x1,rt1), B (lt2,x2,rt2)) -> 
          (* new sub-trees in v1 and v2 *)
          if x1=x2 (* and they are compatible *)
          then B (merge N lt1 lt2, x1, merge N rt1 rt2)
          else raise MergeConflict (* not compatible *)
      | (B (lt,x,rt), B (lt1,x1,rt1), B (lt2,x2,rt2)) ->
          let lt'() = merge lt lt1 lt2 in
          let rt'() = merge rt rt1 rt2 in
          if (x1=x2) then  B (lt'(), x1, rt'())
          else if (x1=x) then B (lt'(), x2, rt'())
          else if (x2=x) then B (lt'(), x1, rt'())
          else raise MergeConflict (* same text edited *)
      | (B _,N, _) | (B _, _, N) -> 
          failwith "Impossible; with tombstones, branch 
                    cannot be deleted."
      | (N,N,N) -> N (* for the sake of completeness *)

end

let main () = 
  let open TreeDoc in 
  let t1 = of_list 
             ["a"; "quick"; "brown"; "fox"; "jumped";
              "over"; "a"; "lazy"; "dog"] in
  let _ = print_doc t1 in 
  let _ = print_doc @@ update t1 [] "flew" in 
  let _ = print_doc @@ update t1 [L; R] "cat" in
  let _ = print_doc @@ insert t1 [L;L;L;L] "thought:" in
  let t2 = insert t1 [L;L;L;L] "thought:" in
  let t3 = insert t1 [L;L;R] "wheatish" in
  let _ = print_doc @@ merge t1 t2 t3 in 
    ();;

main ();;
