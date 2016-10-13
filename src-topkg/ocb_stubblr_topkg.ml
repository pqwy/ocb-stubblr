(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Topkg

let build_arg = Cmd.(v "-plugin-tag" % "package(ocb-stubblr)")
let build_cmd c os = Cmd.(Pkg.build_cmd c os %% build_arg)
let cmd c os files =
  let bc = build_cmd in OS.Cmd.run Cmd.(bc c os %% of_list files)

let mirage ?lib_dst_dir ?(xen=false) ?(fs=false) path =
  let tpath target =
    let (dir, base) = Fpath.(dirname path, basename path) in
    let (name, ext) = Fpath.(rem_ext base, get_ext base) in
    Fpath.("X"//target//dir//name^"+"^target^ext) in
  [ Pkg.clib ?lib_dst_dir ~cond:xen (tpath "mirage-xen");
    Pkg.clib ?lib_dst_dir ~cond:fs (tpath "mirage-freestanding"); ]
