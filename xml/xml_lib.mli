(***********************************************************************)
(*                                                                     *)
(*                  XML Schema Validator Generator                     *)
(*                                                                     *)
(*           Manuel Maarek (SafeRiver)                                 *)
(*                                                                     *)
(*  Copyright 2012, ANSSI and SafeRiver.                               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xml_lib.mli 1646 2012-05-16 08:19:57Z maarek $ *)

val is_silent_mode : Xml.configuration -> bool
val eq_name : Xml.expanded_name -> Xml.expanded_name -> bool
val eq_uri : Xml.expanded_name -> Xml.expanded_name -> bool
val eq_content_elt_uri :
  Stringdata.t -> Xml.expanded_element Xml.content -> bool
val has_attr : Stringdata.t -> Xml.expanded_attributes -> bool
val has_only_xml_xmlns_attrs : Xml.expanded_attributes -> bool
val has_elt :
  Stringdata.t -> Xml.expanded_element Xml.content list -> bool
val get_attr_optional :
  Xml.expanded_name ->
  Xml.expanded_attributes ->
  Stringdata.t option * Xml.expanded_attributes
val get_attr_require :
  Xml.expanded_name ->
  Stringdata.t ->
  Xml.expanded_attributes ->
  Stringdata.t * Xml.expanded_attributes
val get_attr_default :
  Xml.expanded_name ->
  Stringdata.t ->
  Xml.expanded_attributes ->
  Stringdata.t * Xml.expanded_attributes
val get_attr_ignore :
  Xml.expanded_name ->
  Xml.expanded_attributes ->
  'a option * Xml.expanded_attributes
val get_attr_error :
  Xml.expanded_name ->
  Xml.expanded_attributes ->
  'a option * Xml.expanded_attributes
val get_space : Xml.expanded_attributes -> Xml.space
val clear_space_leafs : Xml.space -> 'a Xml.content list -> 'a Xml.content list
val clear_leafs : 'a Xml.content list -> 'a Xml.content list
val next_elt :
  Xml.expanded_name ->
  Xml.expanded_element Xml.content ->
  (Stringdata.t * Xml.expanded_attributes *
   Xml.expanded_element Xml.content list)
  option
val next_elt_required :
  Xml.expanded_name ->
  Xml.expanded_element Xml.content ->
  Stringdata.t * Xml.expanded_attributes *
  Xml.expanded_element Xml.content list
val next_elt_ignore :
  Xml.expanded_name ->
  Xml.expanded_element Xml.content ->
  (Stringdata.t * Xml.expanded_attributes *
   Xml.expanded_element Xml.content list)
  option
val next_elt_error :
  Xml.expanded_name ->
  Xml.expanded_element Xml.content ->
  (Stringdata.t * Xml.expanded_attributes *
   Xml.expanded_element Xml.content list)
  option
val to_string :
    Xml.expanded_name -> string
