module Eg3 = Eg3.Make(struct
                        include Tc.Int
                        let to_string = string_of_int
                        let merge _ = failwith "Integers can't be merged" 
                        module Path = Irmin.Path.String_list
                      end)
module IntVList = Eg3.VList
module IntVPair = Eg3.VPair
module Lwb = Eg3.Lwb


(* let (run : 'a Lwt.t -> 'a) = Lwt_main.run *)
let main () =
  let master = Lwb.master_st in
  let (>>=) a b = let open Lwb in a >>= b in
  let vl = Lwb.run_st master @@
            let l = [1;2;3] in 
            IntVList.add_new [5;6] in
  let wip = Lwb.clone_st "wip" master in
  let vl' = Lwb.run_st master
             begin
               let x = IntVList.to_o vl in
               IntVList.mark_later vl (4::x) >>= fun () ->
               Lwb.commit () >>= fun () ->
               IntVList.get_latest vl >>= fun vl' ->
               Lwb.return vl'
             end in
  let _ = Printf.printf "vl' = [%s]\n" @@ String.concat ";" @@ 
            List.map (string_of_int) (IntVList.to_o vl') in
  let vl'' = Lwb.run_st wip
               begin
                IntVList.mark_later vl [3;5;6] >>= fun () ->
                Lwb.commit () >>= fun () ->
                IntVList.get_latest vl
               end in
  let _ = Printf.printf "vl'' = [%s]\n" @@ String.concat ";" @@ 
            List.map (string_of_int)(IntVList.to_o vl'') in
    ();;

main ();;

