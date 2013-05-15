(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: meta_lib.mli 1365 2012-04-03 15:23:30Z maarek $ *)

val xsd_one_re_of_range : Utf8.range -> Xsd.regexp
val xsd_re_of_range : Utf8.range -> Xsd.regexp
val print_next_charset : string -> Xsd.regexp -> unit
val print_test_charset : string -> Xsd.regexp -> unit
val print_header : string -> unit
