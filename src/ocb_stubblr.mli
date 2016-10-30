(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** OCamlbuild plugin for C stubs

    OCamlbuild plugin for building C stubs. See the {{!intro}Intro},
    {{!utilities}Utilities} and {{!examples}Examples}. *)


(** {1:intro Intro}

    [ocb-stubblr] helps dealing with C libraries, especially ones that provide
    new primitives to OCaml programs (stubs).

    Most of the plugin consists of new tags and rules. In order to activate
    them, {!init} needs to be called from {!Ocamlbuild_plugin.dispatch}:

{[let () = Ocamlbuild_plugin.dispatch Ocb_stubblr.init]}

    The plugin helps with three aspects of building C stubs:

    {2 Using [.clib] files}

    The plugin adds header detection to [.clib] targets, so that the [.c] files
    referenced by a [.clib] are made to depend on any project-local files they
    [#include].

    It also adds the parameterized tag [link_stubs]. Tagging ocaml archives
    ([<foo.cm{,x}a>: link_stubs(path/libbar)]) makes them depend on the C
    libraries produced by the file [path/libbar.clib]. It also adds the
    appropriate [-cclib]/[-dllib] flags when linking the native/bytecode
    archives. This will record the linker flag [-lbar.a] in the produced
    archive, causing the final executables that use it, to also link with the
    stub library.

    {2 [pkg-config]}

    The plugin adds the parameterized tag [pkg-config].

    Tagging objects with [pkg-config(package)] will query [pkg-config] for the
    [package], add its [--cflags] to the compilation of tagged C files, and add
    its [--libs] to the linkage of tagged C and OCaml files.

    For example
{[<src/*.{c,cma,cmxa}>: pkg-config(sdl2)]}
    will add the appropriate flags to compile the C sources with SDL2 includes,
    link them with [libSDL2], and record the flags needed to link with SDL in
    the OCaml archives.

    The full syntax of the tag is [pkg-config(package[,relax][,static])].

    [relax] will ignore the package if it's not found. Without it, the
    compilation will abort.
    [static] will use [--static] instead of [--libs] for the link tags.

    {b Note} [pc] files in the current Opam switch take precedence; see
    {{!Pkg_config}[Pkg_config]}.

    {2:multilib Multi-lib}

    Sometimes it can be desirable to compile a C library in several ways, e.g.
    with different compiler options, and install all of the versions.

    The plugin allows compiling every [.clib] file [path/libstub.clib] as if
    the file [X/<target>/path/libstub+<target>.clib] was also present. It
    will replicate the needed [.c] and [.h] files in the same directory.
    For example, a valid new target is [X/foobar/path/libstub+foobar.a]. This
    new archive is built from the same files as the original one, but it doesn't
    inherit any of its directly applied tags. Instead, the files
    [<X/foobar/**/*>] can be marked with a separate set of tags, in order to
    build and link this archive with different options.

    As a special case, there are two pre-defined targets: [mirage-xen] and
    [mirage-freestanding]. These are already marked with the compilation options
    needed to produce C stubs that work with these two MirageOS backends.

    {b Note} If your paths already contain [+], [OCamlbuild] solver is likely to
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

    [incdirs] controls wheter {{!include_include_dirs}[include_include_dirs]} is
    called. Defaults to [true]. Use [false] to disable.

    [mllibs] passed to {{!ocaml_libs}[ocaml_libs]} to detect [.mllib] files.
    Defaults to [["."]]. Use [[]] to disable. *)

(** {2:utilities Utilities} *)

val ocaml_libs : ?mllibs:path list -> ocb_hook
(** [ocaml_libs ~mllibs] calls {!Ocamlbuild_plugin.ocaml_lib} on every [.mllib]
    found in [mllibs].

    [mllibs] is a list of either files or directories. Directories in the list
    are searched recursively. [mllibs] defaults to [["."]].

    It's a shortcut to enable [use_LIB] tag for every [LIB.mllib].  *)

val include_include_dirs : ocb_hook
(** [include_include_dirs] will add [-I dir] when linking OCaml programs for
    every [dir] marked as [include]. Hence, if the program is compiled against a
    locally-built library that has extra [-dllib] flags, the mentioned stubs
    archives will be correctly resolved. *)

val ccopt : ?tags:string list -> string -> ocb_hook
(** [ccopt tags options] adds [-ccopt options] to compilation of C sources
    tagged with [~tags].

    [tags] defaults to [[]]. *)

val cclib : ?tags:string list -> string -> ocb_hook
(** [cclib tags options] adds [-cclib options] to linkage of C objects tagged
    with [~tags].

    [tags] defaults to [[]]. *)

val ldopt : ?tags:string list -> string -> ocb_hook
(** [ldopt tags options] adds [-ldopt options] when invoking [ocamlmklib] on
    objects tagged with [~tags].

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
    any [pc] files in the current Opam switch take precedence over the
    system-wide ones.

    Queries are cached in the build directory. *)
module Pkg_config : sig

  val run : flags:string list -> string -> [`Nonexistent | `Res of string]
  (** [run ~flags package] queries [pkg-config] about [package], using [flags].

      [`Nonexistent] means that the package was not found. Otherwise,
      [`Res output] is whatever [pkg-config] prints on the standard output.
      
      {b Note} Currently, error in [pkg-config] invocation also results in
      [`Nonexistent]. *)
end

(** {2 OS and machine detection}

    These utilities are included because the host OS and architecture are
    sometimes important when building C stubs. *)

type os = [
  `Linux | `Hurd | `Darwin | `FreeBSD | `OpenBSD | `NetBSD
| `DragonFly | `KFreeBSD | `Haiku | `HP_UX | `AIX | `Interix
| `Minix | `QNX | `SunOS
| `Cygwin of string | `Mingw of string | `Uwin of string | `UNKNOWN of string
]
(** A selection of favourite operating systems. *)

type machine = [ `x86_64 | `x86 |  `ARMv6 | `ARMv7 | `UNKNOWN of string ]
(** A selection of machine architectures. *)

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

    Initialize from [myocamlbuild.ml]:
{[let () = Ocamlbuild_plugin.dispatch Ocb_stubblr.init]}

    The file [src/extra/defs.h] will be automatically used when compiling
    [src/stubs.c], if the latter [#include]s it.

    Adding the tag
{[<src/*.cm{,x}a>: link_stubs(src/libstubs)]}
    will make [foo.cmxa]/[foo.cma] link against [libstubs.a]/[dllstubs.so].

    {2 [pkg-config]}

    Adding the tag
{[<src/*.{c,cma,cmxa}>: pkg-config(sdl2, relax)]}
    will add the appropriate compilation and linkage flags to the C sources and
    archives, and the OCaml archives. If [sdl2] is not installed, it will be
    ignored.

    {2 In-tree executables}

    To build [demo.native] and/or [demo.byte], [_tags] needs to contain

{[<exe/*>: use_foo]}

    This will create the tag [use_foo] (see {{!ocaml_libs}[ocaml_libs]}), causing
    [demo] to link with the [cm{,x}a] archive. The archive in turn contains the
    link flags for the stubs library ([link_stubs]), and [sdl2] ([pkg-config]).
    It will also add [src] to the include path when linking OCaml executables
    (see {{!include_include_dirs}[include_include_dirs]}).

    {2 Multi-lib}

    Invoking [ocamlbuild X/t/src/libstubs+t.a] will build [libstubs+t.a].

    If [_tags] contained
{[<src/*.c>: ccopt(-flub)
<X/t/**/*.c>: ccopt(-DT)]}

    then [libstubs+t.a] would {e not} be compiled with [-flub]. Instead, the
    pre-processor symbol [T] would be defined during compilation.

    {2 Mirage}

    If using [Topkg], register the [.clib] file using
    {!Ocb_stubblr_topkg.mirage}.

{[Pkg.describe ... @ fun c ->
  ...
  Ok ([ Pkg.clib "path/to/libstubs.clib" ] @
        Ocb_stubblr_topkg.mirage "path/to/libstubs.clib")]}

    or directly require individual targets:

{[Pkg.[clib "src/libstubs.clib";
     clib "X/mirage-xen/src/libstubs+mirage-xen.clib";
     clib "X/mirage-freestanding/libstubs+mirage-freestanding.clib"]]}

    Otherwise, arrange for these libraries to be installed.

    When using Mirage, the [META] file still needs to be extended with the
    appropriate link flags, to signal the [mirage] tool to redirect linkage for
    various targets.

    {2 Composition}

{[let myhook = function
  | After_rules -> ...
  | ...]}

{[let () = Ocb_stubblr.(dispatchv [ init; myhook; ])]}

{[let () = dispatch Ocb_stubblr.(init & myhook)]}
*)
