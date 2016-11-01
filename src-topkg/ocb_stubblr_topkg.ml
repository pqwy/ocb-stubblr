(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Topkg

let build_arg = Cmd.(v "-plugin-tag" % "package(ocb-stubblr)")
let build_cmd c os = Cmd.(Pkg.build_cmd c os %% build_arg)
let cmd c os files =
  OS.Cmd.run @@ Cmd.(%%) (build_cmd c os) (Cmd.of_list files)

(* Vs Pkg.clib:
  - .clib file can be generated;
  - installs only the static part (libFOO.a);
  - no ~lib_dst_dir;
  - no source debugging support. *)
let clib_static ?(cond = false) clib =
  if not cond then Pkg.nothing else
  let name = Fpath.(rem_ext clib |> basename) in
  Log.on_error_msg ~use:(fun () -> Pkg.nothing) @@
  if Fpath.get_ext clib = ".clib" && String.is_prefix ~affix:"lib" name then
    let base = String.with_index_range ~first:3 name
    and dir  = Fpath.dirname clib in
    Ok (Pkg.lib ~exts:Exts.c_library Fpath.(dir // "lib" ^ base))
  else R.error_msgf "%s: OCamlbuild .clib must be lib<base>.clib" clib

let mirage ?(xen=false) ?(fs=false) path =
  let tpath target =
    let (dir, base) = Fpath.(dirname path, basename path) in
    let (name, ext) = Fpath.(rem_ext base, get_ext base) in
    Fpath.("X"//target//dir//name^"+"^target^ext) in
  Pkg.flatten [
    clib_static ~cond:xen (tpath "mirage-xen");
    clib_static ~cond:fs (tpath "mirage-freestanding"); ]
