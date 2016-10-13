(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Hook up {!Ocb_stubblr} with [topkg]. *)

open Topkg

(** {1 Activating [ocb-stubblr]} *)

val build_arg : Cmd.t
(** [build_arg] is the command-line parameter that needs to be passed to
    [ocamlbuild] in order to access {!Ocb_stubblr} inside [myocamlbuild.ml]. *)

val build_cmd : Conf.t -> Conf.os -> Cmd.t
(** [build_cmd c os] is the default {!Topkg.build_cmd} extended with
    {{!build_arg}[build_arg]}. *)

val cmd : Conf.t -> Conf.os -> fpath list -> unit result
(** [cmd c os files] is a full invocation of {{!build_cmd}[build_cmd]}. *)

(** {1 Install helpers} *)

val mirage : ?lib_dst_dir:fpath -> ?xen:bool -> ?fs:bool -> fpath -> Pkg.install list
(** [mirage ?lib_dst_dir ?cond ?xen ?fs clib] installs Mirage-specific 
    {{!Ocb_stubblr.multilib}multi-lib} variants of [.clib]. It hands off to
    {!Topkg.Pkg.clib}.

    [lib_dst_dir] is passed to {!clib}.

    [xen] controls [mirage-xen] target. Defaults to [false].

    [fs] controls [mirage-freestanding] target. Defaults to [false]. *)

(** {1 Usage}

    In [pkg.ml]:

    {2 Hooking up [ocb-stubblr]}

{[let build = Pkg.build ~cmd:Ocb_stubblr_topkg.cmd ()

let () = Pkg.describe ~build ...]}


    {2 Mirage}

{[Pkg.describe ... @@ fun c ->
  ...
  Ok ([ ... ] @
  Ocb_stubblr_topkg.clib_mirage
    ~cond ~xen ~fs "path/to/libstubs.clib")]}

*)
