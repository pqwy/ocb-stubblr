(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Ocamlbuild_plugin
open Ocamlbuild_pack
open Outcome
open Astring


let (>>=) a b = match a with Some x -> b x | _ -> None

let strf = Format.asprintf

let error_msgf fmt =
  Format.ksprintf (fun str -> raise (Failure str)) ("Ocb_stubblr: " ^^ fmt)

let error_exit_msgf fmt =
  let k str = Format.printf "%s\n%!" str; exit 1 in
  Format.ksprintf k ("Ocb_stubblr: " ^^ fmt)

let chomp s =
  let drop ~rev s = String.drop ~rev ~sat:Char.Ascii.is_white s in
  drop ~rev:false (drop ~rev:true s)

let memo f =
  let t = Hashtbl.create 13 in fun x ->
    try Hashtbl.find t x with Not_found ->
      let y = f x in Hashtbl.add t x y; y

module Pkg_config = struct

  (* XXX Would be nice to move pkg-config results to a build artefact. *)

  let opam_prefix = lazy (
    try run_and_read "opam config var prefix" |> chomp
    with Failure _ -> error_msgf "error running opam"
  )

  let env () =
    let var  = "PKG_CONFIG_PATH" in
    let vars = [ Lazy.force opam_prefix / "lib" / "pkgconfig" ] @
      try [ Sys.getenv var ] with Not_found -> [] in
    (var, String.concat ~sep:":" vars)

  let run ~flags package =
    let (var, path) = env () in
    let cmd =
      strf "%s=%s pkg-config %s %s 2>/dev/null"
        var path package (String.concat ~sep:" " flags) in
    try `Res (run_and_read cmd |> chomp) with Failure _ -> `Nonexistent

end

let skip_discovered_dir x = x = "_build" || x.[0] = '.'

let path_fold ?(elements = `Any) ?(traverse = `Any) f s roots =
  let rec go acc path =
    let dir = Pathname.is_directory path in
    let acc = match (dir, elements) with
      | (_, `Any) -> f acc path
      | (false, `Files) | (true, `Dirs) -> f acc path
      | (_, `Sat p) when p path -> f acc path
      | _ -> acc in
    let visit = dir && match traverse with
      | `Any -> true | `Sat p -> p path | _ -> false in
    if visit then
      let aux acc name =
        if skip_discovered_dir name then acc else go acc (path/name) in
      Array.fold_left aux acc (Pathname.readdir path)
    else acc in
  List.fold_left go s roots

let find_ext ext paths =
  let f xs x = if Pathname.check_extension x ext then x::xs else xs in
  path_fold ~elements:`Files f [] paths |> List.rev

(* random exportables *)

type path = Pathname.t

type ocb_hook = Ocamlbuild_plugin.hook -> unit

let after_rules f = function After_rules -> f () | _ -> ()

let include_include_dirs = after_rules @@ fun () ->
  flag ["ocaml"; "link"; "program"] @@ S
    List.(map (fun dir -> [A "-I"; A dir]) !Options.include_dirs |> concat)

let ocaml_libs ?(mllibs = ["."]) =
  after_rules @@ fun () ->
    find_ext "mllib" mllibs |> List.iter @@ fun mllib ->
      ocaml_lib (Pathname.remove_extension mllib)

let ccopt_flags ?(tags = []) opts = after_rules @@ fun () ->
  flag (["compile"; "c"] @ tags) (S [A "-ccopt"; A opts])

let cclib_flags ?(tags = []) opts = after_rules @@ fun () ->
  flag (["link"; "c"] @ tags) (S [A "-cclib"; A opts])

let dispatchv hooks = dispatch @@ fun hook -> List.iter (fun f -> f hook) hooks

let (&) f g h = f h; g h

(* os/machine detection *)

type os = [
  `Linux | `Hurd | `Darwin | `FreeBSD | `OpenBSD | `NetBSD
| `DragonFly | `KFreeBSD | `Haiku | `HP_UX | `AIX | `Interix
| `Minix | `QNX | `SunOS
| `Cygwin of string | `Mingw of string | `Uwin of string | `UNKNOWN of string
]

let os () =
  match run_and_read "uname -s" |> chomp with
  | "Linux"        -> `Linux
  | "GNU"          -> `Hurd
  | "Darwin"       -> `Darwin
  | "FreeBSD"      -> `FreeBSD
  | "OpenBSD"      -> `OpenBSD
  | "NetBSD"       -> `NetBSD
  | "DragonFly"    -> `DragonFly
  | "GNU/kFreeBSD" -> `KFreeBSD
  | "Haiku"        -> `Haiku
  | "HP-UX"        -> `HP_UX
  | "AIX"          -> `AIX
  | "Interix"      -> `Interix
  | "Minix"        -> `Minix
  | "QNX"          -> `QNX
  | "SunOS"        -> `SunOS
  | resp ->
      match String.cut ~sep:"-" resp with
      | Some ("CYGWIN_NT", v)  -> `Cygwin v
      | Some ("MINGW32_NT", v) -> `Mingw v
      | Some ("Uwin", v)       -> `Uwin v
      | _                      -> `UNKNOWN resp

type machine = [ `x86_64 | `x86 |  `ARMv6 | `ARMv7 | `UNKNOWN of string ]

let machine () =
  match run_and_read "uname -m" |> chomp with
  | "x86_64" | "amd64" | "i686-64" -> `x86_64
  | "i386" | "i686" -> `x86
  | "armv6l" -> `ARMv6
  | "armv7l" -> `ARMv7
  | resp -> `UNKNOWN resp

(* RULES RULES RULES *)

(* link_stubs(path/to/clib) *)

let link_flag () =
  let tag = "link_stubs" in
  let libarg switch clib =
    let name = Pathname.(remove_extension clib |> basename) in
    let name = String.(if is_prefix ~affix:"lib" name then drop ~max:3 name else name) in
    S [A switch; A ("-l"^name)]
  and dep flag = Pathname.([remove_extension flag -.- "a"]) in
  pflag ["link"; "ocaml"; "library"; "byte"] tag (libarg "-dllib");
  pflag ["link"; "ocaml"; "library"; "native"] tag  (libarg "-cclib");
  pdep ["link"; "ocaml"] tag dep;
  pdep ["compile"; "ocaml"] tag dep
  (* XXX sneak in '-I' for compile;ocaml;program ?? *)

(* *.c depends on *.h *)

let cdeps c deps env _ =
  let c = env c and deps = env deps in
  (* XXX FIXME FRAGILE: figure out the actual source dir. *)
  let root = ".." in
  let cmd = Cmd (S [A "cd"; P root; Sh "&&"; A "cc"; A "-MM"; A "-MG"; P c]) in
  let to_list str =
    String.fields ~empty:false ~is_sep:Char.Ascii.is_white str
      |> List.filter ((<>) "\\") in
  let lines = match Command.to_string cmd |> run_and_read |> to_list with
    | _::_::xs -> List.map (fun p -> Pathname.normalize p ^ "\n") xs
    | _ -> error_exit_msgf "%s: depends: unrecognized format" c in
  Echo (lines, deps)

let cc_rules () =

  rule "ocaml C stubs: c -> c.depends"
    ~dep:"%.c" ~prod:"%.c.depends" (cdeps "%.c" "%.c.depends");

  let x_o = "%"-.-(!Options.ext_obj) in
  (* XXX
   * The original OCamlbuild action for building [c -> o]. Keep in sync with
   * their [src/ocaml_specific.ml].
   * *)
  let default_action env _build =
    let c = env "%.c" in
    let o = env x_o in
    let comp = if Tags.mem "native" (tags_of_pathname c) then !Options.ocamlopt else !Options.ocamlc in
    let cc = Cmd(S[comp; T(tags_of_pathname c++"c"++"compile"); A"-c"; Px c]) in
    if Pathname.dirname o = Pathname.current_dir_name then cc
    else Seq[cc; mv (Pathname.basename o) o]
  in
  let action env build =
    let deps = string_list_of_file (env "%.c.depends")
    and check_outcome = function
      | Good _ -> ()
      | Bad _  -> () in
      (* XXX We ignore errors here because we can't tell the difference
       * between external and internal includes. *)
      (* | Bad exn  -> error_msgf "building %s: %s" c (Printexc.to_string exn) in *)
    build (List.map (fun p -> [p]) deps) |> List.iter check_outcome;
    default_action env build in
  rule "ocaml C stubs: c & c.depends -> o"
    ~prod:x_o
    ~deps:["%.c"; "%.c.depends"]
    ~doc:"The OCaml compiler can be passed .c files and will call \
          the underlying C toolchain to produce corresponding .o files. \
          ocamlc or ocamlopt will be used depending on whether \
          the 'native' flag is set on the .c file.\
          (Extended version, taking #includes into account.)"
    action

(* pkg-config(package[,relax][,static]) *)

let pkgconf_args argstr =
  match String.cuts ~empty:false ~sep:"," argstr |> List.map chomp with
  | x::xs -> (x, List.mem "relax" xs, List.mem "static" xs)
  | _ -> error_msgf "pkg-config(%s): malformed arguments" argstr

let run_pkgconf = memo @@ fun package ->
  let run f = match Pkg_config.run ~flags:[f] package with
    | `Res x -> Some x | _ -> None in
  run "--cflags" >>= fun cflags ->
  run "--libs" >>= fun libs ->
  run "--static" >>= fun static ->
    Some (cflags, libs, static)

let get_pkgconf argstring =
  let (package, relax, static) = pkgconf_args argstring in
  match run_pkgconf package with
  | None when relax -> None
  | None -> error_msgf "pkg-config: package %s not found" package
  | Some (cf, _, st) when static -> Some (cf, st)
  | Some (cf, lb, _) -> Some (cf, lb)

let pkg_conf_flag () =
  let tag = "pkg-config" in
  let pkgconf p f args =
    match get_pkgconf args with Some r when p r <> "" -> f (p r) | _ -> S [] in
  pflag ["c"; "compile"] tag
    (pkgconf fst (fun cflags -> S [A "-ccopt"; A cflags]));
  pflag ["ocaml"; "link"] tag
    (pkgconf snd (fun libs -> S [A "-cclib"; A libs]));
  pflag ["c"; "ocamlmklib"] tag
    (pkgconf snd (fun libs -> A libs))

(* let () = *)
(*   rule "get pkg-config" ~prod:"%(package).pkg-config" *)
(*   (fun env _ -> *)
(*     let pkg = env "%(package)" *)
(*     and dst = env "%(package).pkg-config" in *)
(*     let cmd = S [A "pkg-config"; A pkg] in *)
(*     Seq [Cmd (S [cmd; A "--cflags"; Sh ">"; Px dst]); *)
(*          Cmd (S [cmd; A "--libs"; Sh ">>"; Px dst]); *)
(*          Cmd (S [cmd; A "--static"; Sh ">>"; Px dst])]) *)

(* multi-lib *)

let x_cdeps src dst target env _ =
  let target = env target
  and paths = string_list_of_file (env src) in
  Echo (List.map (fun p -> "X"/target/p^"\n") paths, env dst)

let x_rules () =
  rule "multi-lib: .deps"
    ~dep:"%(path).c.depends" ~prod:"X/%(target)/%(path).c.depends"
    (x_cdeps "%(path).c.depends" "X/%(target)/%(path).c.depends" "%(target)");
  copy_rule "multi-lib: .c" "%(path).c" "X/%(target)/%(path).c";
  copy_rule "multi-lib: .h" "%(path).h" "X/%(target)/%(path).h";
  copy_rule "multi-lib: .clib" "%(path).clib" "X/%(target)/%(path)+%(target).clib";
  Configuration.parse_string
    "<X/mirage-xen/**>: pkg-config(mirage-xen, relax, static)";
  Configuration.parse_string
    "<X/mirage-freestanding/**>: pkg-config(ocaml-freestanding, relax, static)"

(* activate *)

let rules = function
| Before_rules ->
    link_flag ();
    pkg_conf_flag ();
    x_rules ();
    cc_rules ();
| _ -> ()

let ignore _ = ()

let init ?(incdirs=true) ?mllibs =
  rules
  & ocaml_libs ?mllibs
  & (if incdirs then include_include_dirs else ignore)
