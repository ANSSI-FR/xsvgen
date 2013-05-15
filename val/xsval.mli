(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xsval.mli 1453 2012-04-24 15:55:52Z maarek $ *)

(** {1 XSD validating table interface} *)

val xml_configuration : Xml.configuration
val schema : Lxsd.resolved_schema
