(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xml_lexer.mli 1654 2012-05-22 17:21:28Z maarek $ *)

val lex_xml : Xml.error_status -> Stringdata.t -> Xml.declaration * Xml.stream

val is_spaces : Stringdata.t -> bool
val is_ncname : Stringdata.t -> bool

val lex_qname : Stringdata.t -> Xml.unexpanded_name option
val lex_qname_ns : Xml_ns.t * Stringdata.t -> Xml.expanded_name option
val lex_list : Stringdata.t -> Stringdata.t list
