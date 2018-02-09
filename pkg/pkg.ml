#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "utext" @@ fun c ->
  Ok [ Pkg.mllib "src/utext.mllib";
       Pkg.lib "src/utext_top_init.ml";
       Pkg.test "test/test"; ]
