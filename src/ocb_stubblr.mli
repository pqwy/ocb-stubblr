(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** OCamlbuild plugin for C stubs

    See the {{!intro}Intro}, {{!interface}Interface} or {{!examples}Examples}. *)


(** {1:intro Intro}

    [ocb-stubblr] helps dealing with C libraries that are built as part of an
    OCaml project. It is especially useful for libraries of OCaml primitives
    (stubs).

    Most of the plugin consists of new tags that can be applied to files, and
    new rules that are activated when OCamlbuild tries to build certain targets.
    In order to enable these, {!init} needs to be called from
    {!Ocamlbuild_plugin.dispatch}:

{[let () = Ocamlbuild_plugin.dispatch Ocb_stubblr.init]}

    The plugin helps with three aspects of building C stubs:

    {2 1. Using [.clib] files}

    [stubblr] modifies [.clib] build rules to automatically search for, and
    require, project-local headers that the C source files [#include].

    Furthermore, it provides the tag [link_stubs()]. This tag acts on OCaml
    archives, and records the link flags needed for linking with the given C
    library. The parameter is assumed to be a [.clib] file in the same project.

    For example, adding
{[<foo.cm{,x}a>: link_stubs(path/libbar)]}
    records the link flag [-lbar] in [foo.cm{,x}a]. Assuming that the C
    libraries described by [path/libbar.clib] are installed, this causes any
    final executables that use the archive [foo.cmxa] (resp. [foo.cma]) to link
    to [libbar.a] (resp [dllbar.so]). This is useful if [Foo] provides the
    interface to the C primitives in [libbar].

    Another feature is the automatic addition of [use_<lib>] tags for every
    [<lib>.mllib]. OCaml sources tagged with this tag are built against the
    archive [<lib>.cm{,x}a], instead of using its constituent [cm{o,x}] through
    [include] tags. This means that the in-tree executables inherit the link
    flags (as introduced, for example, above), and are correctly linked against
    the in-tree C libraries.

    Finally, the tags [ccopt] and [cclib], which were introduced in OCamlbuild
    0.9.3, are added if the plugin is used with an older version. This allows
    setting these options directly from [_tags] with any OCamlbuild version.

    {2 2. [pkg-config]}

    [stubblr] provides the tag [pkg-config()].

    Tagging objects with [pkg-config(package)] will query [pkg-config] for the
    [package], and:

    {ol
    {- add the C flags ([pkg-config --cflags]) to the compilation of tagged C
       sources;}
    {- add the link flags ([pkg-config --libs]) to the linking step of tagged C
       libraries; and}
    {- record those link flags in the tagged OCaml archives.}}

    For example
{[<src/*.{c,cma,cmxa}>: pkg-config(sdl2)]}
    will add the flags needed to compile against SDL2 when compiling C sources,
    and record the flags needed to link the final executables against [libSDL2.so]
    to the native and bytecode archives.

    The full syntax of the tag is [pkg-config(package[,relax][,static])].

    [relax] will ignore the package if it's not found. Without it, the
    compilation will abort.

    [static] will use [--static] instead of [--libs] for the link flags.

    {b Note} [.pc] files in the current Opam switch take precedence; see
    {{!Pkg_config}[Pkg_config]}.

    {2:multilib 3. Multi-lib}

    Sometimes it can be desirable to compile a C library in several ways, e.g.
    with different compilation options, and install all of the versions.


    For any file [path/libstub.clib], [stubblr] introduces the rules to build
    [X/<TARGET>/path/libstub+<TARGET>.clib], where [<TARGET>] is an arbitrary
    name. The new library is built from the same sources, but it doesn't inherit
    any of the tags directly applied to the original [.clib] and its products.
    Instead, the files [X/<TARGET>/**/*] can be marked with a separate set of
    tags, causing them to be compiled and/or linked with different options.

    As a special case, there are pre-defined targets for different MirageOS
    runtimes (currently [mirage-xen] and [mirage-freestanding]). These are
    automatically tagged with the required compilation options.

    {b Note} If your paths already contain [+], OCamlbuild solver is likely to
    get confused. Assume that the meaning of [+] in paths has been hijacked by
    [ocb-stubblr]. The new semantics of [+] is accessible only through
    transcendental hermenautics.

    *)


(** {1:interface Interface} *)

open Ocamlbuild_plugin

type ocb_hook = Ocamlbuild_plugin.hook -> unit

type path = Pathname.t

val init : ?incdirs:bool -> ?mllibs:path list -> ocb_hook
(** [init ?incdirs ?paths] initializes the plugin.

    [incdirs] causes {{!include_include_dirs}[include_include_dirs]} to be
    called on initialisation. Defaults to [true].

    [mllibs] are passed to {{!ocaml_libs}[ocaml_libs]}, to detect any
    [<lib>.mllib] files and enable their corresponding [use_<lib>] tags.
    Use [[]] to disable. Defaults to [["."]]. *)

(** {2:utilities Utilities} *)

val ocaml_libs : ?mllibs:path list -> ocb_hook
(** [ocaml_libs ~mllibs] calls {!Ocamlbuild_plugin.ocaml_lib} on every [.mllib]
    found in [mllibs]. It's a shortcut to enable [use_<lib>] tag for every
    [<lib>.mllib] in the project.

    [mllibs] is a list of files or directories. Directories in the list are
    searched recursively. [mllibs] defaults to [["."]]. *)

val include_include_dirs : ocb_hook
(** [include_include_dirs] will add [-I dir] when linking OCaml programs for
    every [dir] marked as [include]. *)

val ccopt : ?tags:string list -> string -> ocb_hook
(** [ccopt tags options] adds [-ccopt options] when compiling the C sources
    tagged with [~tags].

    [tags] defaults to [[]]. *)

val cclib : ?tags:string list -> string -> ocb_hook
(** [cclib tags options] adds [-cclib options] when linking the C libraries
    tagged with [~tags].

    [tags] defaults to [[]]. *)

val ldopt : ?tags:string list -> string -> ocb_hook
(** [ldopt tags options] adds [-ldopt options] when linking the C libraries
    tagged with [~tags].

    [tags] defaults to [[]]. *)

val after_rules : (unit -> unit) -> ocb_hook
(** [after_rules f] is [function After_rules -> f () | _ -> ()]. *)

val dispatchv : ocb_hook list -> unit
(** [dispatchv hooks] is a shortcut for registering several
    {{!ocb_hook}ocb_hooks}.

    It is equivalent to [Ocamlbuild_plugin.dispatch hookf] where [hookf] is a
    function that applies each hook from [hooks] in order. *)

val (&) : ocb_hook -> ocb_hook -> ocb_hook
(** [h1 & h2] is a hook combining [h1] and [h2]. *)

(** Query [pkg-config].

    [pkg-config] is invoked with the environment extended with the equivalent of
    {[PKG_CONFIG_PATH=$(opam config var prefix)/lib/pkgconfig]} This means that
    any [.pc] files in the current Opam switch take precedence over the
    system-wide ones. *)
module Pkg_config : sig

  val run : flags:string list -> string -> [`Nonexistent | `Res of string]
  (** [run ~flags package] queries [pkg-config] about [package], using [flags].

      [`Nonexistent] means that the package was not found. Otherwise,
      [`Res output] is whatever [pkg-config] prints on the standard output.
      
      {b Note} Currently, all errors in [pkg-config] invocation results in
      [`Nonexistent]. *)
end

(** {2 OS and machine detection}

    These utilities are included because it is sometimes necessary to change
    options for building C libraries depending on the host OS and architecture. *)

type os = [
  `Linux | `Hurd | `Darwin | `FreeBSD | `OpenBSD | `NetBSD
| `DragonFly | `KFreeBSD | `Haiku | `HP_UX | `AIX | `Interix
| `Minix | `QNX | `SunOS
| `Cygwin of string | `Mingw of string | `Uwin of string | `UNKNOWN of string
]
(** A selection of popular operating systems. *)

type machine = [ `x86_64 | `x86 |  `ARMv6 | `ARMv7 | `UNKNOWN of string ]
(** A selection of machine architectures supported by OCaml. *)

val os : unit -> os
(** [os ()] is the normalized result of [uname -s]. *)

val machine : unit -> machine
(** [machine ()] is the normalized result of [uname -m]. *)

(** {1:examples Examples}

    Assume a project laid out like the following:

    Project dir:
{[./myocamlbuild.ml
./_tags
./src/foo.ml
./src/stubs.c
./src/extra/defs.h
./src/libstubs.clib
./src/foo.mllib
./exe/demo.ml]}

    The content of [src/foo.mllib]:
{[Foo]}

    The content of [src/libstubs.clib]:
{[stubs.o]}

    The content of [_tags]:
{[<src>: include]}

    {2 Basic integration}

    Initialize the plugin from [myocamlbuild.ml]:
{[let () = Ocamlbuild_plugin.dispatch Ocb_stubblr.init]}

    The file [src/extra/defs.h] will be automatically used when compiling
    [src/stubs.c], if needed.

    Adding the tag
{[<src/*.cm{,x}a>: link_stubs(src/libstubs)]}
    will record the link flag [-lstubs] in [foo.cm{,x}a], causing executables
    that use them to link against [libstubs.a]/[dllstubs.so].

    {2 [pkg-config]}

    Adding the tag
{[<src/*.{c,cma,cmxa}>: pkg-config(sdl2, relax)]}
    will cause [stubs.c] to be compiled with C flags from [sdl2.pc], and
    [foo.cm{,x}a] to record the link flags.

    If [SDL2] is not installed, [relax] will cause it to be ignored.

    {2 In-tree executables}

    To build [demo.native] and/or [demo.byte], [_tags] needs to contain

{[<exe/*>: use_foo]}

    causing [demo.{native,byte}] to build against [foo.cm{,x}a] and inherit its
    link flags. The archive, in turn, contains flags for linking to the stub
    library ([link_stubs()]), and linking to [SDL2] ([pkg-config()]).

    {2 Multi-lib}

    Invoking [ocamlbuild X/fnord/src/libstubs+fnord.a] will build
    [libstubs+fnord.a].

    If [_tags] contains
{[<src/*.c>: ccopt(-flub)
<X/fnord/**/*.c>: ccopt(-DA)]}

    then [libstubs+fnord.a] will {e not} be compiled with [-flub]. Instead, it
    will be compiled with the pre-processor symbol [A] defined.

    {2 Mirage}

    If using [Topkg], register the [.clib] file using
    {!Ocb_stubblr_topkg.mirage}.

{[Pkg.describe ... @ fun c ->
  ...
  Ok [ Pkg.clib "path/to/libstubs.clib";
       Ocb_stubblr_topkg.mirage "path/to/libstubs.clib"]]}

    Otherwise, arrange for building and installation of
    [X/<TARGET>/path/to/libstubs+<TARGET>.a] for all MirageOS [TARGET]s.

    Use of these alternate archives is a matter of MirageOS.

    {2 Composition}

{[let myhook = function
  | After_rules -> ...
  | ...]}

{[let () = Ocb_stubblr.(dispatchv [init; myhook])]}

{[let () = dispatch Ocb_stubblr.(init & myhook)]}
*)
