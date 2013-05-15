(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_generator.mli 1501 2012-05-02 09:42:38Z maarek $ *)

val generate_ml : Format.formatter -> Xml.configuration -> Lxsd.resolved_schema -> unit
val generate_sh : Format.formatter -> File_out.prepared -> unit

