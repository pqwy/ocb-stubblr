(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Ocamlbuild_plugin
open Ocamlbuild_pack
open Outcome
open Astring


let (>>=) a b = match a with Some x -> b x | _ -> None

let error_msgf fmt =
  Format.ksprintf (fun str -> raise (Failure str)) ("Ocb_stubblr: " ^^ fmt)

let error_exit_msgf fmt =
  let k str = Format.printf "%s\n%!" str; exit 1 in
  Format.ksprintf k ("Ocb_stubblr: " ^^ fmt)

let chomp s =
  let drop ~rev s = String.drop ~rev ~sat:Char.Ascii.is_white s in
  drop ~rev:false (drop ~rev:true s)

let run_and_read cmd = run_and_read cmd |> chomp

let memo f =
  let t = Hashtbl.create 13 in fun x ->
    try Hashtbl.find t x with Not_found ->
      let y = f x in Hashtbl.add t x y; y

module Pkg_config = struct

  (* XXX Would be nice to move pkg-config results to a build artefact. *)

  let opam_prefix =
    let cmd = "opam config var prefix" in
    lazy ( try run_and_read cmd with Failure _ ->
            error_msgf "error running opam")

  let var = "PKG_CONFIG_PATH"

  let path () =
    let opam = Lazy.force opam_prefix
    and rest = try [Sys.getenv var] with Not_found -> [] in
    opam/"lib"/"pkgconfig" :: opam/"share"/"pkgconfig" :: rest
      |> String.concat ~sep:":"

  let run ~flags package =
    let cmd = strf "%s=%s pkg-config %s %s 2>/dev/null"
              var (path ()) package (String.concat ~sep:" " flags) in
    try `Res (run_and_read cmd) with Failure _ -> `Nonexistent
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
  let inc = S List.(!Options.include_dirs
    |> map (fun dir -> [A "-I"; A dir]) |> concat) in
  flag ["ocaml"; "link"; "program"] inc;
  flag ["ocaml"; "link"; "extension:cmxs"] inc

let ocaml_libs ?(mllibs = ["."]) =
  after_rules @@ fun () ->
    find_ext "mllib" mllibs |> List.iter @@ fun mllib ->
      ocaml_lib (Pathname.remove_extension mllib)

let ccopt ?(tags = []) opts = after_rules @@ fun () ->
  flag (["compile"; "c"] @ tags) (S [A "-ccopt"; A opts])

let cclib ?(tags = []) opts = after_rules @@ fun () ->
  flag (["link"; "c"] @ tags) (S [A "-cclib"; A opts])

let ldopt ?(tags = []) opts = after_rules @@ fun () ->
  flag (["c"; "ocamlmklib"] @ tags) (S [A "-ldopt"; A opts])

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
  match run_and_read "uname -s" with
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
  match run_and_read "uname -m" with
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
  pflag ["link"; "ocaml"; "library"] tag  (libarg "-cclib");
  pdep ["link"; "ocaml"] tag dep;
  pdep ["compile"; "ocaml"] tag dep
  (* XXX sneak in '-I' for compile;ocaml;program ?? *)

(* *.c depends on *.h *)

(* Source dir is caught as `cwd` at module-init time. *)
let root = Unix.getcwd ()

let cdeps c deps env _ =
  let c = env c and deps = env deps in
  let to_list str = List.filter ((<>) "\\") @@
    String.fields ~empty:false ~is_sep:Char.Ascii.is_white str in
  let cmd = Cmd (
    S [ A "cd"; P root; Sh "&&";
        A "cc"; T (tags_of_pathname c); A "-MM"; A "-MG"; P c ]) in
  let headers = match Command.to_string cmd |> run_and_read |> to_list with
    | _::_::xs -> List.map (fun p -> Pathname.normalize p ^ "\n") xs
    | _ -> error_exit_msgf "%s: depends: unrecognized format" c in
  (* XXX Prepend dirname to unresolved headers? *)
  Echo (headers, deps)

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
    ~prod:x_o ~deps:["%.c"; "%.c.depends"]
    (* XXX [~doc] was introduced in OCaml 4.02. *)
    (* ~doc:"The OCaml compiler can be passed .c files and will call \ *)
    (*       the underlying C toolchain to produce corresponding .o files. \ *)
    (*       ocamlc or ocamlopt will be used depending on whether \ *)
    (*       the 'native' flag is set on the .c file.\ *)
    (*       (Extended version, taking #includes into account.)" *)
    action

(* pkg-config(package[,relax[,param...]]) *)

let run_pkgconf = memo @@ fun package ->
  let run flags = match Pkg_config.run ~flags package with
    | `Res x -> Some x | _ -> None in
  run ["--cflags"] >>= fun cflags ->
  run ["--libs"] >>= fun libs ->
  run ["--libs"; "--static"] >>= fun static ->
    Some (cflags, libs, static)

let pkgconf_args argstr =
  match String.cuts ~empty:false ~sep:" " argstr with
  | x::xs ->
      let (relax, flags) = List.partition ((=) "relax") xs in
      (x, List.mem "relax" relax, flags)
  | _ -> error_msgf "pkg-config(%s): malformed arguments" argstr

let get_pkgconf argstring =
  let (package, relax, flags) = pkgconf_args argstring in
  let flag x = List.mem x flags
  and some = function "" -> None | fl -> Some fl in
  match run_pkgconf package with
  | None when relax -> None
  | None -> error_msgf "pkg-config: package %s not found" package
  | Some (cf, lb, lb_st) ->
      let cf = if flag "cflags" || flags = [] then some cf else None
      and lb =
        if flag "static" then some lb_st else
        if flag "libs" || flags = [] then some lb else None in
      Some (cf, lb)

let pkg_conf_flag () =
  let tag = "pkg-config" in
  let pkgconf p f args =
    match get_pkgconf args >>= p with Some x -> f x | _ -> S [] in
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
  rule "multi-lib: derive .c.depends"
    ~dep:"%(path).c.depends" ~prod:"X/%(target)/%(path).c.depends"
    (x_cdeps "%(path).c.depends" "X/%(target)/%(path).c.depends" "%(target)");
  copy_rule "multi-lib: cp .c" "%(path).c" "X/%(target)/%(path).c";
  copy_rule "multi-lib: cp .h" "%(path).h" "X/%(target)/%(path).h";
  copy_rule "multi-lib: cp .clib" "%(path).clib" "X/%(target)/%(path)+%(target).clib"

let mirage_rules () = let open Configuration in
  (* Mirage itself takes care of the linkage. *)
  parse_string "<X/mirage-xen/**>: pkg-config(mirage-xen-ocaml relax cflags)";
  parse_string "<X/mirage-freestanding/**>: pkg-config(ocaml-freestanding relax cflags)"

(* back-ports of 0.9.3 flags *)

let backported_c_flags () =
  let vs = ["4.01"; "4.02"] in
  if List.exists (fun affix -> String.is_prefix ~affix Sys.ocaml_version) vs
  (* Inject flags if the OCaml version is known to have been shipped with
     bundled ocamlbuild. We can't detect the three stand-alone versions of
     ocamlbuild that lack them, 0.9.0, 0.9.1 and 0.9.2. *)
  then begin
    pflag ["c"; "compile"] "ccopt" (fun param -> S [A "-ccopt"; A param]);
    pflag ["c"; "link"] "ccopt" (fun param -> S [A "-ccopt"; A param]);
    pflag ["c"; "compile"] "cclib" (fun param -> S [A "-cclib"; A param]);
    pflag ["c"; "link"] "cclib" (fun param -> S [A "-cclib"; A param]);
  end

(* activate *)

let rules = function
| Before_rules ->
    link_flag ();
    pkg_conf_flag ();
    backported_c_flags ();
    x_rules (); (* multi-lib c.depends takes precedence *)
    cc_rules ();
    mirage_rules ();
| _ -> ()

let ignore _ = ()

let init ?(incdirs=true) ?mllibs =
  rules
  & ocaml_libs ?mllibs
  & (if incdirs then include_include_dirs else ignore)
