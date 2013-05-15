(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xml_ns.mli 1648 2012-05-16 14:11:36Z maarek $ *)

type t
val default : t
val from_list : (Stringdata.t option * Stringdata.t) list -> t
val add_unexp_attrs : Xml.unexpanded_attributes -> t -> t
val add_exp_attrs : Xml.expanded_attributes -> t -> t
val expand_elt_opt : Xml.unexpanded_name -> t -> Xml.expanded_name option
val expand_elt : Xml.unexpanded_name -> t -> Xml.expanded_name
val expand_attr_opt : Xml.unexpanded_name -> t -> Xml.expanded_name option
val expand_attr : Xml.unexpanded_name -> t -> Xml.expanded_name
val empty_expanded_ns : Stringdata.t
