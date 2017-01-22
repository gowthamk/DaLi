open Lwt
open Irmin_unix
module Store =
  Irmin_git.FS (Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)

let config = Irmin_git.config ~root:"/tmp/irmin/test" ~bare:true ()
let prog =
  Store.Repo.create config >>= Store.master task >>= fun t ->
  Store.update (t "Updating foo/bar")  ["foo"; "bar"] "hi!" >>= fun () ->
  Store.read_exn (t "Reading foo/bar") ["foo"; "bar"] >>= fun x ->
  Printf.printf "Read: %s\n%!" x;
  Lwt.return_unit
let () = Lwt_main.run prog
