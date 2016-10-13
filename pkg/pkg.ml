#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "ocb-stubblr" @@ fun c ->
  Ok [ Pkg.mllib "src/ocb-stubblr.mllib";
       Pkg.mllib "src-topkg/ocb-stubblr-topkg.mllib";
  ]
