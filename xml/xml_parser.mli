(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xml_parser.mli 1341 2012-03-26 17:18:13Z maarek $ *)

val parse :
    Xml.configuration -> Xml.declaration * Xml.stream -> Xml.expanded_tree
