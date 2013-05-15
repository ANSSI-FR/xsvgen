(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xsd_lineariser.mli 1452 2012-04-24 14:49:31Z maarek $ *)

val linearise : (Xsd.inlined, Xsd.uri) Xsd.schema -> Lxsd.resolved_schema

