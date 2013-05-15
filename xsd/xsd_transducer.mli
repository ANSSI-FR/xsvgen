(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xsd_transducer.mli 1419 2012-04-16 14:35:45Z maarek $ *)

val transduce :
    Xml.expanded_tree -> (Xsd.composite,Xsd.uri option) Xsd.schema
val inline :
    (Xsd.composite,Xsd.uri option) Xsd.schema ->
      (Xsd.uri * (Xsd.composite, Xsd.uri option) Xsd.schema)
        Xsd_lib.M_composite.t ->
          (Xsd.inlined, Xsd.uri) Xsd.schema
